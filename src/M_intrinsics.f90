!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_intrinsics
implicit none
private
public help_intrinsics
!interface help_intrinsics
!   module procedure help_intrinsics_all
!   module procedure help_intrinsics_one
!end interface help_intrinsics
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)                       :: name
logical,intent(in),optional                       :: prefix
logical,intent(in),optional                       :: topic
logical,intent(in),optional                       :: m_help
character(len=256),allocatable                    :: textblock(:)
character(len=:),allocatable                      :: a, b, c
integer                                           :: i, p, pg
   select case(name)
   case('','manual','intrinsics','fortranmanual','fortran_manual')
      textblock=help_intrinsics_all(prefix,topic,m_help)
   case('fortran','toc')
      textblock=help_intrinsics_section()
      do i=1,size(textblock)
         p = index(textblock(i), '[')
         pg = index(textblock(i), ']')
         if(p.gt.0.and.pg.gt.p)then
          a=textblock(i)(:p-1)
          b=textblock(i)(p:pg)
          c=textblock(i)(pg+1:)
          textblock(i)=b//' '//a//c
         endif
      enddo
      call sort_name(textblock)
   case default
      textblock=help_intrinsics_one(name,prefix,topic,m_help)
   end select
end function help_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_section() result (textblock)

!@(#) grab lines in NAME section and append them to generate an index of manpages

character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: add(:)
character(len=256),allocatable  :: label
character(len=10)               :: cnum
integer                         :: i
integer                         :: icount
logical                         :: is_label
logical                         :: grab
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum)
      if( size(add) .eq. 0 ) exit
      label=''
      grab=.false.
      is_label=.false.
      do i=1,size(add)
         if(add(i).ne.'')then
            is_label=verify(add(i)(1:1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ') == 0
         endif
         if(is_label.and.add(i).eq.'NAME')then
            grab=.true.
         elseif(is_label)then
            exit
         elseif(grab)then
            label=adjustl(trim(label))//' '//adjustl(trim(add(i)))
         endif
      enddo
      textblock=[character(len=256) :: textblock,label]
      icount=icount + 1
   enddo
end function help_intrinsics_section
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_all(prefix,topic,m_help) result (textblock)
logical,intent(in),optional     :: prefix
logical,intent(in),optional     :: topic
logical,intent(in),optional     :: m_help
character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: header(:)
character(len=256),allocatable  :: add(:)
character(len=10)               :: cnum
integer                         :: icount
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum,prefix,topic,m_help)
      if( size(add) .eq. 0 ) exit
      textblock=[character(len=256) :: textblock,add]
      icount=icount + 1
   enddo
   if(present(m_help))then
      if(m_help)then
         header=[ character(len=256) :: &
         '================================================================================',    &
         'SUMMARY',    &
         ' The primary Fortran topics are',    &
         ' abs                   achar                     acos',    &
         ' acosh                 adjustl                   adjustr',    &
         ' aimag                 aint                      all',    &
         ' allocated             anint                     any',    &
         ' asin                  asinh                     associated',    &
         ' atan                  atan2                     atanh',    &
         ' atomic_add            atomic_and                atomic_cas',    &
         ' atomic_define         atomic_fetch_add          atomic_fetch_and',    &
         ' atomic_fetch_or       atomic_fetch_xor          atomic_or',    &
         ' atomic_ref            atomic_xor                backspace',    &
         ' bessel_j0             bessel_j1                 bessel_jn',    &
         ' bessel_y0             bessel_y1                 bessel_yn',    &
         ' bge                   bgt                       bit_size',    &
         ' ble                   block                     blt',    &
         ' btest                 c_associated              ceiling',    &
         ' c_f_pointer           c_f_procpointer           c_funloc',    &
         ' char                  c_loc                     close',    &
         ' cmplx                 co_broadcast              co_lbound',    &
         ' co_max                co_min                    command_argument_count',    &
         ' compiler_options      compiler_version          conjg',    &
         ' continue              co_reduce                 cos',    &
         ' cosh                  co_sum                    co_ubound',    &
         ' count                 cpu_time                  cshift',    &
         ' c_sizeof              date_and_time             dble',    &
         ' digits                dim                       dot_product',    &
         ' dprod                 dshiftl                   dshiftr',    &
         ' eoshift               epsilon                   erf',    &
         ' erfc                  erfc_scaled               event_query',    &
         ' execute_command_line  exit                      exp',    &
         ' exponent              extends_type_of           findloc',    &
         ' float                 floor                     flush',    &
         ' fraction              gamma                     get_command',    &
         ' get_command_argument  get_environment_variable  huge',    &
         ' hypot                 iachar                    iall',    &
         ' iand                  iany                      ibclr',    &
         ' ibits                 ibset                     ichar',    &
         ' ieor                  image_index               include',    &
         ' index                 int                       ior',    &
         ' iparity               is_contiguous             ishft',    &
         ' ishftc                is_iostat_end             is_iostat_eor',    &
         ' kind                  lbound                    leadz',    &
         ' len                   len_trim                  lge',    &
         ' lgt                   lle                       llt',    &
         ' log                   log10                     log_gamma',    &
         ' logical               maskl                     maskr',    &
         ' matmul                max                       maxexponent',    &
         ' maxloc                maxval                    merge',    &
         ' merge_bits            min                       minexponent',    &
         ' minloc                minval                    mod',    &
         ' modulo                move_alloc                mvbits',    &
         ' nearest               new_line                  nint',    &
         ' norm2                 not                       null',    &
         ' num_images            pack                      parity',    &
         ' popcnt                poppar                    precision',    &
         ' present               product                   radix',    &
         ' random_number         random_seed               range',    &
         ' rank                  real                      repeat',    &
         ' reshape               return                    rewind',    &
         ' rrspacing             same_type_as              scale',    &
         ' scan                  selected_char_kind        selected_int_kind',    &
         ' selected_real_kind    set_exponent              shape',    &
         ' shifta                shiftl                    shiftr',    &
         ' sign                  sin                       sinh',    &
         ' size                  sngl                      spacing',    &
         ' spread                sqrt                      stop',    &
         ' storage_size          sum                       system_clock',    &
         ' tan                   tanh                      this_image',    &
         ' tiny                  trailz                    transfer',    &
         ' transpose             trim                      ubound',    &
         ' unpack                verify',    &
         '']
         textblock=[header,textblock]
      endif
   endif
end function help_intrinsics_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_one(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)      :: name
logical,intent(in),optional      :: prefix
logical,intent(in),optional      :: m_help
logical,intent(in),optional      :: topic
character(len=256),allocatable   :: textblock(:)
character(len=:),allocatable     :: shortname
integer                          :: i
select case(name)

case('1','abs')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   abs(3f) - [FORTRAN:INTRINSIC:NUMERIC] Absolute value', &
'', &
'SYNTAX', &
'   result = ABS(A)', &
'', &
'DESCRIPTION', &
'   abs(A) computes the absolute value of numeric argument A.', &
'', &
'ARGUMENTS', &
'  A    the type of the argument shall be an INTEGER, REAL, or', &
'       COMPLEX scalar or array with INTENT(IN).', &
'', &
'RETURN VALUE', &
'   If A is of type INTEGER or REAL, the value of the result is |A| and', &
'   of the same type and kind as the input argument.', &
'', &
'   if A is COMPLEX with value (X, Y), the result is a REAL equal to', &
'   a processor-dependent approximation to SQRT(X**2 + Y**2) computed', &
'   without undue overflow or underflow.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'     program demo_abs', &
'     integer :: i = -1, iout', &
'     real :: x = -1.e0, xout, zout', &
'     complex :: z = (-3.e0,4.e0)', &
'     doubleprecision :: r8 = 45.78D+00, dout', &
'        write(*,*)''INPUTS:'',i,x,z,r8', &
'        iout = abs(i)', &
'        xout = abs(x)', &
'        zout = abs(z)', &
'        dout = abs(r8)', &
'        write(*,*)''OUTPUTS:'',iout,xout,zout,dout', &
'        write ( *, ''(a,f12.4,12x,f12.4)'' ) '' Double precision  '', -r8, abs(r8)', &
'        ! COMPLEX', &
'        ! 3 - 4 -5 right triangle test :', &
'        write(*,*)''The abs() of (3.0,4.0) should be 5.0'',abs((3.0,4.0))', &
'        ! ELEMENTAL', &
'        write(*,*)''abs is ELEMENTAL: '',abs([-10, 20, 0, -1, -3, 100])', &
'     end program demo_abs', &
'  Results:', &
'', &
'    INPUTS:  -1  -1.00000000 (-3.00000000,4.00000000)   45.780000000000001', &
'    OUTPUTS:  1   1.00000000 5.00000000                 45.780000000000001', &
'    Double precision -45.7800 45.7800', &
'    The abs() of (3.0,4.0) should be 5.0   5.00000000', &
'    abs is ELEMENTAL: 10 20 0 1 3 100', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   Elemental function', &
' JSU', &
'']

shortname="abs"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('2','achar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   achar(3f) - [FORTRAN:INTRINSIC:CHARACTER] returns a character in a specified', &
'   position in the ASCII collating sequence', &
'', &
'SYNTAX', &
'   result = ACHAR(I [, KIND])', &
'', &
'DESCRIPTION', &
'   achar(I) returns the character located at position I in the ASCII', &
'   collating sequence.', &
'', &
'   The ADEs (ASCII Decimal Equivalents) for ASCII are', &
'', &
'    *-------*-------*-------*-------*-------*-------*-------*-------*', &
'    | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|', &
'    | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |', &
'    | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|', &
'    | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |', &
'    | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  '' |', &
'    | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |', &
'    | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |', &
'    | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |', &
'    | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |', &
'    | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |', &
'    | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |', &
'    | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |', &
'    | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |', &
'    |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |', &
'    |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |', &
'    |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|', &
'    *-------*-------*-------*-------*-------*-------*-------*-------*', &
'', &
'ARGUMENTS', &
'   I       the type shall be INTEGER.', &
'   KIND    (optional) an INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type CHARACTER with a length of one. If the', &
'   KIND argument is present, the return value is of the specified kind', &
'   and of the default kind otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_achar', &
'    implicit none', &
'    character(len=1) :: c', &
'    integer,parameter :: blank=32', &
'    integer,parameter :: horizonal_tab=11', &
'    integer,parameter :: escape=27', &
'    integer :: i', &
'      c = achar(blank)', &
'      write(*,''(i0,1x,a,1x,b0,1x,o0,1x,z0)'')blank,c,c,c,c', &
'      write(*,''(32(a))'') (achar(i),i=32,126)', &
'    end program demo_achar', &
'', &
'  Results:', &
'', &
'   32   100000 40 20', &
'    !"#$%&''()*+,-./0123456789:;<=>?', &
'   @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_', &
'   `abcdefghijklmnopqrstuvwxyz{|}~', &
'', &
'NOTE', &
'   see [[ichar]] for a discussion of converting between numerical', &
'   values and formatted string representations.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, with KIND argument Fortran 2003 and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   char(3), iachar(3), ichar(3)', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   elemental:  adjustl(3), adjustr(3), index(3), len_trim(3), scan(3), verify(3)', &
'   nonelemental:  repeat(3), trim(3)', &
'']

shortname="achar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('3','acos')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   acos(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] arccosine function', &
'', &
'SYNTAX', &
'   result = ACOS(X)', &
'', &
'DESCRIPTION', &
'   acos(X) computes the arccosine of X (inverse of cos(x)).', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL with a magnitude that is', &
'        less than one.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X.', &
'   The real part of the result is in radians and lies in the range', &
'', &
'      0 <= ACOS(X) <= PI.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_acos', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128', &
'   implicit none', &
'   real(kind=real64) :: x = 0.866_real64', &
'   real(kind=real64),parameter :: D2R=acos(-1.0_real64)/180.0_real64', &
'     write(*,*)''acos('',x,'') is '', acos(x)', &
'     write(*,*)''90 degrees is '', d2r*90.0_real64, '' radians''', &
'     write(*,*)''180 degrees is '', d2r*180.0_real64, '' radians''', &
'     write(*,*)''for reference &', &
'     &PI= 3.14159265358979323846264338327950288419716939937510''', &
'    end program demo_acos', &
'', &
'  Results:', &
'', &
'    acos(  0.86599999999999999      ) is   0.52364958093182890', &
'    90 degrees is    1.5707963267948966       radians', &
'    180 degrees is    3.1415926535897931       radians', &
'    for reference PI= 3.14159265358979323846264338327950288419716939937510', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later; for a complex argument [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   Inverse function: cos(3)', &
'']

shortname="acos"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('4','acosh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   acosh(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Inverse', &
'   hyperbolic cosine function', &
'', &
'SYNTAX', &
'   result = acosh(x)', &
'', &
'DESCRIPTION', &
'   ACOSH(X) computes the inverse hyperbolic cosine of X.', &
'', &
'ARGUMENTS', &
'   X    the type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has the same type and kind as X. If X is complex,', &
'   the imaginary part of the result is in radians and lies between', &
'', &
'      0 <= AIMAG(ACOSH(X)) <= PI.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_acosh', &
'    implicit none', &
'    real(8), dimension(3) :: x = [ 1.0, 2.0, 3.0 ]', &
'      write (*,*) acosh(x)', &
'    end program demo_acosh', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   Inverse function: cosh(3)', &
'']

shortname="acosh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('5','adjustl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   adjustl(3f) - [FORTRAN:INTRINSIC:CHARACTER] Left-adjust a string', &
'', &
'SYNTAX', &
'   result = adjustl(string)', &
'', &
'     character(len=*),intent(in) :: string', &
'     character(len=len(string))  :: result', &
'', &
'DESCRIPTION', &
'   adjustl(STRING) will left adjust a string by removing leading spaces.', &
'   Spaces are inserted at the end of the string as needed.', &
'', &
'ARGUMENTS', &
'   STRING    the type shall be CHARACTER.', &
'', &
'RETURN VALUE', &
'   The return value is of type CHARACTER and of the same kind as', &
'   STRING where leading spaces are removed and the same number of', &
'   spaces are inserted on the end of STRING.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_adjustl', &
'    implicit none', &
'    character(len=20) :: str = ''   sample string''', &
'    character(len=:),allocatable :: astr', &
'       !', &
'       ! basic use', &
'       str = adjustl(str)', &
'       write(*,''("[",a,"]")'') str, trim(str)', &
'       !', &
'       ! an allocatable string stays the same length', &
'       ! and is not trimmed.', &
'       astr=''    allocatable string   ''', &
'       write(*,''("[",a,"]")'') adjustl(astr)', &
'       !', &
'    end program demo_adjustl', &
'', &
'  Results:', &
'', &
'   [sample string       ]', &
'   [sample string]', &
'   [allocatable string       ]', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   nonelemental:  repeat(3), trim(3)', &
' JSU', &
'']

shortname="adjustl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('6','adjustr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   adjustr(3f) - [FORTRAN:INTRINSIC:CHARACTER] Right-adjust a string', &
'', &
'SYNTAX', &
'   result = ADJUSTR(STRING)', &
'', &
'     character(len=*),intent(in) :: string', &
'     character(len=len(string))  :: result', &
'', &
'DESCRIPTION', &
'   adjustr(STRING) will right adjust a string by removing trailing spaces.', &
'   Spaces are inserted at the start of the string as needed.', &
'', &
'ARGUMENTS', &
'   STRING    the type shall be CHARACTER.', &
'', &
'RETURN VALUE', &
'   The return value is of type CHARACTER and of the same kind as STRING', &
'   where trailing spaces are removed and the same number of spaces are', &
'   inserted at the start of STRING.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_adjustr', &
'    implicit none', &
'    integer :: right', &
'    character(len=*),parameter :: bracket=''("[",a,"]")''', &
'    character(len=20) :: str = '' sample string ''', &
'    character(len=:),allocatable :: astr ', &
'       call number_line()', &
'       !', &
'       ! basic usage', &
'       str = adjustr(str)', &
'       write(*,bracket) str', &
'', &
'       ! exploring usage:', &
'       ! An allocatable string and arbitrary margin.', &
'       ! Set a right margin and adjust to it. Note', &
'       ! this would truncate if the margin is less', &
'       ! than the length of STR', &
'       right=50', &
'       astr=adjustr(str//repeat('' '',max(0,right-len(str))))', &
'       write(*,bracket) astr ', &
'       !', &
'       call number_line()', &
'       !', &
'    contains', &
'       subroutine number_line()', &
'       ! print a short number line', &
'          write(*,bracket)repeat(''1234567890'',5)', &
'       end subroutine number_line', &
'    end program demo_adjustr', &
'', &
'  Results:', &
'', &
'   [12345678901234567890123456789012345678901234567890]', &
'   [       sample string]', &
'   [                                     sample string]', &
'   [12345678901234567890123456789012345678901234567890]', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   nonelemental:  repeat(3), trim(3)', &
'', &
' JSU', &
'']

shortname="adjustr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('7','aimag')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   aimag(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Imaginary part of complex', &
'   number', &
'', &
'SYNTAX', &
'   result = AIMAG(Z)', &
'', &
'DESCRIPTION', &
'   AIMAG(Z) yields the imaginary part of complex argument Z.', &
'', &
'ARGUMENTS', &
'   Z    The type of the argument shall be COMPLEX.', &
'', &
'RETURN VALUE', &
'', &
'   The return value is of type REAL with the kind type parameter of', &
'   the argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_aimag', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    complex(kind=real32) z4', &
'    complex(kind=real64) z8', &
'       z4 = cmplx(1.e0, 2.e0)', &
'       z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)', &
'       print *, aimag(z4), aimag(z8)', &
'       ! an elemental function can be passed an array', &
'       print *', &
'       print *, [z4,z4/2.0,z4+z4,z4**3]', &
'       print *', &
'       print *, aimag([z4,z4/2.0,z4+z4,z4**3])', &
'    end program demo_aimag', &
'', &
'  Results:', &
'', &
'      2.000000       4.00000000000000     ', &
'', &
'    (1.000000,2.000000) (0.5000000,1.000000) (2.000000,4.000000)', &
'    (-11.00000,-2.000000)', &
'', &
'      2.000000       1.000000       4.000000      -2.000000    ', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'']

shortname="aimag"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('8','aint')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   aint(3f) - [FORTRAN:INTRINSIC:NUMERIC] Truncate to a whole number', &
'', &
'SYNTAX', &
'   result = AINT(A [, KIND])', &
'', &
'DESCRIPTION', &
'   AINT(A [, KIND]) truncates its argument to a whole number.', &
'', &
'ARGUMENTS', &
'   A       the type of the argument shall be REAL.', &
'   KIND    (optional) an INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL with the kind type parameter of the', &
'   argument if the optional KIND is absent; otherwise, the kind type', &
'   parameter will be given by KIND. If the magnitude of X is less', &
'   than one, aint(x) returns zero. If the magnitude is equal to or', &
'   greater than one then it returns the largest whole number that does', &
'   not exceed its magnitude. The sign is the same as the sign of X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_aint', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real ::  x4', &
'    real(kind=real64) :: x8', &
'       x4 = 1.234E0_4', &
'       x8 = 4.321_real64', &
'       print *, aint(x4), dint(x8)', &
'       x8 = aint(x4,kind=real64)', &
'    end program demo_aint', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'']

shortname="aint"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('9','all')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   all(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] determines if all the', &
'   values are true', &
'', &
'SYNTAX', &
'   result = ALL(MASK [, DIM])', &
'', &
'DESCRIPTION', &
'   Logical conjunction of elements of MASK along dimension DIM.', &
'', &
'   "ALL(MASK [, DIM])" determines if all the values are true in MASK', &
'   in the array along dimension DIM.', &
'', &
'ARGUMENTS', &
'   MASK    shall be a logical array. That is,', &
'           the type of the argument shall be LOGICAL and it shall', &
'           not be scalar.', &
'   DIM     (optional) DIM shall be a scalar integer with a value', &
'           that lies between one and the rank of MASK.', &
'           The corresponding actual argument shall not be an optional', &
'           dummy argument.', &
'', &
'RETURN VALUE', &
'   "ALL(MASK)" returns a scalar value of type LOGICAL where the kind', &
'   type parameter is the same as the kind type parameter of MASK. If', &
'   DIM is present, then ALL(MASK, DIM) returns an array with the rank', &
'   of MASK minus 1. The shape is determined from the shape of MASK', &
'   where the DIM dimension is elided.', &
'', &
'    1. ALL(MASK) is true if all elements of MASK are true.', &
'       It also is true if MASK has zero size; otherwise, it is false.', &
'', &
'    2. If the rank of MASK is one, then ALL(MASK, DIM) is equivalent', &
'       to ALL(MASK). If the rank is greater than one, then ALL(MASK,', &
'       DIM) is determined by applying ALL to the array sections.', &
'', &
'    4. Result Characteristics. The result is of type logical with the same', &
'       kind type parameter as MASK. It is scalar', &
'       if DIM is absent or n = 1; otherwise, the result has rank n - 1 and', &
'       shape [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ] where [d1 ,', &
'       d2 , . . . , dn ] is the shape of MASK.', &
'', &
'    5. Result Value.', &
'', &
'       Case (i):   The result of ALL (MASK) has the value true if all', &
'                   elements of MASK are true or if MASK has', &
'                   size zero, and the result has value false if any element', &
'                   of MASK is false.', &
'', &
'       Case (ii):  If MASK has rank one, ALL(MASK,DIM) is equal to', &
'                   ALL(MASK). Otherwise, the value of element', &
'                   (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn ) of ALL', &
'                   (MASK, DIM) is equal to ALL (MASK (s1 , s2 , . . . ,', &
'                   sDIM-1 , :, sDIM+1 , . . . , sn )).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_all', &
'    implicit none', &
'    logical l', &
'       l = all([.true., .true., .true.])', &
'       print *, l', &
'       call section', &
'    contains', &
'       subroutine section', &
'       integer a(2,3), b(2,3)', &
'          a = 1', &
'          b = 1', &
'          b(2,2) = 2', &
'          print *, all(a .eq. b, 1)', &
'          print *, all(a .eq. b, 2)', &
'       end subroutine section', &
'    end program demo_all', &
'', &
'  Case (i):', &
'', &
'     The value of ALL ([.TRUE., .FALSE., .TRUE.]) is false.', &
'', &
'  Case (ii):', &
'', &
'     >                        1|3|5', &
'     > If B is the array      -+-+-', &
'     >                        2|4|6', &
'     >', &
'     >                        0|3|5', &
'     > and C is the array     -+-+-', &
'     >                        7|4|8', &
'', &
'     then ALL (B /= C, DIM = 1) is', &
'', &
'        [true, false, false]', &
'', &
'     and ALL (B /= C, DIM = 2) is', &
'', &
'        [false, false].', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function.', &
'', &
'']

shortname="all"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('10','allocated')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   allocated(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] Status of an', &
'   allocatable entity', &
'', &
'SYNTAX', &
'   * result = ALLOCATED(ARRAY)', &
'   * result = ALLOCATED(SCALAR)', &
'', &
'DESCRIPTION', &
'   ALLOCATED(ARRAY) and ALLOCATED(SCALAR) check the allocation status of', &
'   ARRAY and SCALAR, respectively.', &
'', &
'ARGUMENTS', &
'   ARRAY     the argument shall be an ALLOCATABLE array.', &
'   SCALAR    the argument shall be an ALLOCATABLE scalar.', &
'', &
'RETURN VALUE', &
'   The return value is a scalar LOGICAL with the default logical', &
'   kind type parameter. If the argument is allocated then the result', &
'   is .true.; otherwise, it returns .false..', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_allocated', &
'    implicit none', &
'    integer :: i = 4', &
'    real(4), allocatable :: x(:)', &
'       if (allocated(x) .eqv. .false.) allocate(x(i))', &
'    end program demo_allocated', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later. Note, the scalar= keyword and allocatable', &
'   scalar entities are available in Fortran 2003 and later.', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   move_alloc(3)', &
'', &
'']

shortname="allocated"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('11','anint')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   anint(3f) - [FORTRAN:INTRINSIC:NUMERIC] Nearest whole number', &
'', &
'SYNTAX', &
'   result = anint(a [, kind])', &
'', &
'DESCRIPTION', &
'   anint(a [, kind]) rounds its argument to the nearest whole number.', &
'', &
'ARGUMENTS', &
'   A       the type of the argument shall be REAL.', &
'   KIND    (optional) an INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type real with the kind type parameter of the', &
'   argument if the optional KIND is absent; otherwise, the kind type', &
'   parameter will be given by KIND. If A is greater than zero, anint(a)', &
'   returns aint(x + 0.5). If A is less than or equal to zero then it', &
'   returns aint(x - 0.5).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_anint', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real(kind=real32) :: x4', &
'    real(kind=real64) :: x8', &
'       x4 = 1.234E0_real32', &
'       x8 = 4.321_real64', &
'       print *, anint(x4), dnint(x8)', &
'       x8 = anint(x4,kind=real64)', &
'    end program demo_anint', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="anint"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('12','any')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   any(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] determines if any of the', &
'   values in the logical array are true.', &
'', &
'SYNTAX', &
'   result = any(mask [, dim])', &
'', &
'DESCRIPTION', &
'   ANY(MASK [, DIM]) determines if any of the values in the logical', &
'   array MASK along dimension DIM are .TRUE..', &
'', &
'ARGUMENTS', &
'   MASK    the type of the argument shall be LOGICAL and', &
'           it shall not be scalar.', &
'   DIM     (optional) DIM shall be a scalar integer with a value', &
'           that lies between one and the rank of MASK.', &
'', &
'RETURN VALUE', &
'   ANY(MASK) returns a scalar value of type LOGICAL where the kind', &
'   type parameter is the same as the kind type parameter of MASK. If', &
'   DIM is present, then ANY(MASK, DIM) returns an array with the rank', &
'   of MASK minus 1. The shape is determined from the shape of MASK', &
'   where the DIM dimension is elided.', &
'', &
'   1. ANY(MASK) is true if any element of MASK is true; otherwise, it', &
'      is false. It also is false if MASK has zero size.', &
'', &
'   2. If the rank of MASK is one, then ANY(MASK, DIM) is equivalent', &
'      to ANY(MASK). If the rank is greater than one, then ANY(MASK,', &
'      DIM) is determined by applying ANY to the array sections.', &
'', &
'EXAMPLE', &
'Sample program:', &
'', &
'    program demo_any', &
'    implicit none', &
'    logical l', &
'       l = any([.true., .true., .true.])', &
'       print *, l', &
'       call section', &
'       contains', &
'         subroutine section', &
'         integer a(2,3), b(2,3)', &
'           a = 1', &
'           b = 1', &
'           b(2,2) = 2', &
'           print *, any(a .eq. b, 1)', &
'           print *, any(a .eq. b, 2)', &
'         end subroutine section', &
'    end program demo_any', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="any"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('13','asin')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   asin(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Arcsine function', &
'', &
'SYNTAX', &
'   result = asin(X)', &
'', &
'DESCRIPTION', &
'   asin(x) computes the arcsine of its argument X.', &
'', &
'   The arcsine is the inverse function of the sine function. It is', &
'   commonly used in trigonometry when trying to find the angle when the', &
'   lengths of the hypotenuse and the opposite side of a right triangle', &
'   are known.', &
'', &
'ARGUMENTS', &
'   X    The type shall be either REAL and a magnitude that is less', &
'        than or equal to one; or be COMPLEX.', &
'', &
'RETURN VALUE', &
'   RESULT  The return value is of the same type and kind as X.', &
'           The real part of the result is in radians and lies in the', &
'           range -PI/2 <= asin(x) <= PI/2.', &
'', &
'EXAMPLE', &
'   The arcsine will allow you to find the measure of a right angle when', &
'   you know the ratio of the side opposite the angle to the hypotenuse.', &
'', &
'   So if you knew that a train track rose 1.25 vertical miles on a', &
'   track that was 50 miles long, you could determine the average angle', &
'   of incline of the track using the arcsine. Given', &
'', &
'    sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)', &
'', &
'    program demo_asin', &
'    use, intrinsic :: iso_fortran_env, only : dp=>real64', &
'    implicit none', &
'    ! value to convert degrees to radians', &
'    real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp', &
'    real(kind=dp)           :: angle, rise, run', &
'      ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)', &
'      ! then taking the arcsine of both sides of the equality yields', &
'      ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)', &
'      rise=1.250_dp', &
'      run=50.00_dp', &
'      angle = asin(rise/run)', &
'      write(*,*)''angle of incline(radians) = '', angle', &
'      angle = angle/D2R', &
'      write(*,*)''angle of incline(degrees) = '', angle', &
'', &
'      write(*,*)''percent grade='',rise/run*100.0_dp', &
'    end program demo_asin', &
'  Results:', &
'', &
'    angle of incline(radians) =    2.5002604899361139E-002', &
'    angle of incline(degrees) =    1.4325437375665075', &
'    percent grade=   2.5000000000000000', &
'', &
'  The percentage grade is the slope, written as a percent. To calculate', &
'  the slope you divide the rise by the run. In the example the rise', &
'  is 1.25 mile over a run of 50 miles so the slope is 1.25/50 =', &
'  0.025. Written as a percent this is 2.5 %.', &
'', &
'  for the US, two and 1/2 percent is generally thought of as the upper', &
'  limit. This means a rise of 2.5 feet when going 100 feet forward. In', &
'  the US this was the maximum grade on the first major US railroad,', &
'  the Baltimore and Ohio. Note curves increase the frictional drag on', &
'  a train reducing the allowable grade.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, for a complex argument [[Fortran 2008]] or later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   Inverse function: sin(3)', &
'JSU', &
'']

shortname="asin"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('14','asinh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   asinh(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Inverse', &
'   hyperbolic sine function', &
'', &
'SYNTAX', &
'    result = asinh(x)', &
'', &
'DESCRIPTION', &
'   asinh(x) computes the inverse hyperbolic sine of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X. If X is complex,', &
'   the imaginary part of the result is in radians and lies between -PI/2', &
'   <= AIMAG(ASINH(X)) <= PI/2.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_asinh', &
'   implicit none', &
'   real(8), dimension(3) :: x = [ -1.0, 0.0, 1.0 ]', &
'      write (*,*) asinh(x)', &
'   end program demo_asinh', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   Inverse function: sinh(3)', &
'', &
'']

shortname="asinh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('15','associated')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   associated(3f) - [FORTRAN:INTRINSIC] Status of a pointer or', &
'   pointer/target pair', &
'', &
'SYNTAX', &
'   result = associated(pointer [, target])', &
'', &
'DESCRIPTION', &
'   associated(pointer [, target]) determines the status of the pointer', &
'   POINTER or if POINTER is associated with the target TARGET.', &
'', &
'ARGUMENTS', &
'   POINTER    POINTER shall have the POINTER attribute', &
'              and it can be of any type.', &
'   TARGET     (Optional) TARGET shall be a pointer or', &
'              a target. It must have the same type, kind type parameter, and', &
'              array rank as POINTER.', &
'', &
'   The association status of neither POINTER nor TARGET shall be', &
'   undefined.', &
'', &
'RETURN VALUE', &
' associated(pointer) returns a scalar value of type logical(4). There are', &
' several cases:', &
'', &
' 1. When the optional TARGET is not present then', &
'    associated(pointer) is true if POINTER is associated with a', &
'    target; otherwise, it returns false.', &
'', &
' 2. If TARGET is present and a scalar target, the result is true if', &
'    TARGET is not a zero-sized storage sequence and the target', &
'    associated with POINTER occupies the same storage units. If', &
'    POINTER is disassociated, the result is false.', &
'', &
' 3. If TARGET is present and an array target, the result is true if', &
'    TARGET and POINTER have the same shape, are not zero-sized', &
'    arrays, are arrays whose elements are not zero-sized storage', &
'    sequences, and TARGET and POINTER occupy the same storage', &
'    units in array element order.', &
'', &
'    As in case 2, the result is false, if POINTER is disassociated.', &
'', &
' 4. If TARGET is present and an scalar pointer, the result is true', &
'    if TARGET is associated with POINTER, the target associated', &
'    with TARGET are not zero-sized storage sequences and occupy the', &
'    same storage units.', &
'', &
'    The result is false, if either TARGET or POINTER is', &
'    disassociated.', &
'', &
' 5. If TARGET is present and an array pointer, the result is true if', &
'    target associated with POINTER and the target associated with', &
'    TARGET have the same shape, are not zero-sized arrays, are', &
'    arrays whose elements are not zero-sized storage sequences, and', &
'    TARGET and POINTER occupy the same storage units in array', &
'    element order. The result is false, if either TARGET or', &
'    POINTER is disassociated.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_associated', &
'    implicit none', &
'    real, target  :: tgt(2) = [1., 2.]', &
'    real, pointer :: ptr(:)', &
'       ptr => tgt', &
'       if (associated(ptr)     .eqv. .false.) &', &
'       & stop ''POINTER NOT ASSOCIATED''', &
'       if (associated(ptr,tgt) .eqv. .false.) &', &
'       & stop ''POINTER NOT ASSOCIATED TO TARGET''', &
'    end program demo_associated', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   null(3)', &
'']

shortname="associated"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('16','atan2')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atan2(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Arctangent', &
'   function', &
'', &
'SYNTAX', &
'   result = atan2(y, x)', &
'', &
'DESCRIPTION', &
'   atan2(y, x) computes the arctangent of the complex number', &
'', &
'      X + i Y.', &
'', &
'   This function can be used to transform from Cartesian into polar', &
'   coordinates and allows to determine the angle in the correct quadrant.', &
'   To convert from Cartesian Coordinates (x,y) to polar coordinates', &
'', &
'   (r,theta):', &
'   $$', &
'   \begin{aligned}', &
'   r &= \sqrt{x**2 + y**2} \\', &
'   \theta &= \tan**{-1}(y / x)', &
'   \end{aligned}', &
'   $$', &
'', &
'ARGUMENTS', &
'   Y    The type shall be REAL.', &
'   X    The type and kind type parameter shall be the same as Y.', &
'        If Y is zero, then X must be nonzero.', &
'', &
'RETURN VALUE', &
'   The return value has the same type and kind type parameter as Y.', &
'   It is the principal value of the complex number (X + i, Y). If', &
'   X is nonzero, then it lies in the range -PI <= atan(x) <= PI.', &
'   The sign is positive if Y is positive. If Y is zero, then', &
'   the return value is zero if X is strictly positive, PI if', &
'   X is negative and Y is positive zero (or the processor does', &
'   not handle signed zeros), and -PI if X is negative and', &
'   Y is negative zero. Finally, if X is zero, then the', &
'   magnitude of the result is PI/2.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atan2', &
'   implicit none', &
'   real(4) :: x = 1.e0_4, y = 0.5e0_4', &
'      x = atan2(y,x)', &
'   end program demo_atan2', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'']

shortname="atan2"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('17','atan')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atan(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Arctangent', &
'   function', &
'', &
'SYNTAX', &
'   * result = atan(x)', &
'   * result = atan(y, x)', &
'', &
'DESCRIPTION', &
'   atan(x) computes the arctangent of X.', &
'', &
'ARGUMENTS', &
'  X    The type shall be REAL or COMPLEX; if Y is present, X', &
'       shall be REAL.', &
'  Y    Shall be of the same type and kind as X.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X.', &
'   If Y is present, the result is identical to atan2(y,x).', &
'   Otherwise, it the arc tangent of X, where the real part of', &
'   the result is in radians and lies in the range', &
'', &
'   -PI/2 <= atan(x) <= PI/2.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atan', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'   implicit none', &
'   real(kind=real64) :: x = 2.866_real64', &
'      x = atan(x)', &
'   end program demo_atan', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later;', &
'   for a complex argument and for two arguments [[Fortran 2008]] or later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   atan2(3), tan(3)', &
'']

shortname="atan"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('18','atanh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atanh(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Inverse', &
'   hyperbolic tangent function', &
'', &
'SYNTAX', &
'   result = atanh(x)', &
'', &
'DESCRIPTION', &
'   atanh(x) computes the inverse hyperbolic tangent of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as X. If X is', &
'   complex, the imaginary part of the result is in radians and lies between', &
'', &
'   -PI/2 <= AIMAG(ATANH(X)) <= PI/2.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_atanh', &
'    implicit none', &
'    real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]', &
'       write (*,*) atanh(x)', &
'    end program demo_atanh', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   Inverse function: tanh(3)', &
'', &
'']

shortname="atanh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('19','atomic_add')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_add(3f) - [FORTRAN:INTRINSIC:ATOMIC] Atomic ADD operation', &
'', &
'SYNTAX', &
'   call atomic_add (atom, value [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_add(atom, value) atomically adds the value of VAR to the', &
'   variable ATOM. When STAT is present and the invocation was', &
'   successful, it is assigned the value 0. If it is present and the', &
'   invocation has failed, it is assigned a positive value; in particular,', &
'   for a coindexed ATOM, if the remote image has stopped, it is', &
'   assigned the value of iso_fortran_env''s stat_stopped_image and if', &
'   the remote image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer', &
'           type with atomic_int_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind', &
'           is different, the value is converted to the kind of ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_add', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*]', &
'      call atomic_add (atom[1], this_image())', &
'   end program demo_atomic_add', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_fetch_add(3), iso_fortran_env(3),', &
'   atomic_and(3), atomic_or(3), atomic_xor(3)', &
'', &
'']

shortname="atomic_add"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('20','atomic_and')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_and(3f) - [FORTRAN:INTRINSIC:ATOMIC:BIT MANIPULATION] Atomic bitwise', &
'   AND operation', &
'', &
'SYNTAX', &
'   call atomic_and(atom, value [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_and(atom, value) atomically defines ATOM with the bitwise', &
'   AND between the values of ATOM and VALUE. When STAT is present', &
'   and the invocation was successful, it is assigned the value 0. If it', &
'   is present and the invocation has failed, it is assigned a positive', &
'   value; in particular, for a coindexed ATOM, if the remote image has', &
'   stopped, it is assigned the value of iso_fortran_env''s', &
'   stat_stopped_image and if the remote image has failed, the value', &
'   stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM    Scalar coarray or coindexed variable of integer type with', &
'          atomic_int_kind kind.', &
'  VALUE   Scalar of the same type as ATOM. If the kind is', &
'          different, the value is converted to the kind of ATOM.', &
'  STAT    (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_and', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*]', &
'      call atomic_and(atom[1], int(b''10100011101''))', &
'   end program demo_atomic_and', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_fetch_and(3), iso_fortran_env(3),', &
'   atomic_add(3), atomic_or(3), atomic_xor(3)', &
'', &
'']

shortname="atomic_and"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('21','atomic_cas')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_cas(3f) - [FORTRAN:INTRINSIC:ATOMIC] Atomic compare and swap', &
'', &
'SYNTAX', &
'   call atomic_cas (atom, old, compare, new [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_cas compares the variable ATOM with the value of COMPARE;', &
'   if the value is the same, ATOM is set to the value of', &
'   NEW. Additionally, OLD is set to the value of ATOM that was used', &
'   for the comparison. When STAT is present and the invocation was', &
'   successful, it is assigned the value 0. If it is present and the', &
'   invocation has failed, it is assigned a positive value; in particular,', &
'   for a coindexed ATOM, if the remote image has stopped, it is', &
'   assigned the value of iso_fortran_env''s stat_stopped_image and if', &
'   the remote image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM       Scalar coarray or coindexed variable of either integer type', &
'             with atomic_int_kind kind or logical type with', &
'             atomic_logical_kind kind.', &
'  OLD        Scalar of the same type and kind as ATOM.', &
'  COMPARE    Scalar variable of the same type and kind as ATOM.', &
'  NEW        Scalar variable of the same type as ATOM. If kind is', &
'             different, the value is converted to the kind of ATOM.', &
'  STAT       (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_cas', &
'   use iso_fortran_env', &
'   implicit none', &
'   logical(atomic_logical_kind) :: atom[*], prev', &
'      call atomic_cas(atom[1], prev, .false., .true.)', &
'   end program demo_atomic_cas', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_ref(3), iso_fortran_env(3)', &
'', &
'']

shortname="atomic_cas"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('22','atomic_define')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_define(3f) - [FORTRAN:INTRINSIC:ATOMIC] Setting a variable atomically', &
'', &
'SYNTAX', &
'   call atomic_define (atom, value [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_define(atom, value) defines the variable ATOM with the', &
'   value VALUE atomically. When STAT is present and the invocation', &
'   was successful, it is assigned the value 0. If it is present and the', &
'   invocation has failed, it is assigned a positive value; in particular,', &
'   for a coindexed ATOM, if the remote image has stopped, it is', &
'   assigned the value of iso_fortran_env''s stat_stopped_image and if', &
'   the remote image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of either integer type', &
'           with atomic_int_kind kind or logical type with', &
'           atomic_logical_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind is', &
'           different, the value is converted to the kind of ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_define', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*]', &
'      call atomic_define(atom[1], this_image())', &
'   end program demo_atomic_define', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later; with STAT, [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_ref(3), atomic_cas(3), iso_fortran_env(3),', &
'   atomic_add(3), atomic_and(3), atomic_or(3), atomic_xor(3)', &
'']

shortname="atomic_define"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('23','atomic_fetch_add')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_fetch_add(3f) - [FORTRAN:INTRINSIC:ATOMIC] Atomic ADD operation', &
'   with prior fetch', &
'', &
'SYNTAX', &
'   call atomic_fetch_add(atom, value, old [, stat])', &
'', &
'DESCRIPTION', &
'', &
'   atomic_fetch_add(atom, value, old) atomically stores the value of', &
'   ATOM in OLD and adds the value of VAR to the variable', &
'   ATOM. When STAT is present and the invocation was successful, it', &
'   is assigned the value 0. If it is present and the invocation has', &
'   failed, it is assigned a positive value; in particular, for a', &
'   coindexed ATOM, if the remote image has stopped, it is assigned the', &
'   value of iso_fortran_env''s stat_stopped_image and if the remote', &
'   image has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer type with', &
'           atomic_int_kind kind. atomic_logical_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind is', &
'           different, the value is converted to the kind of ATOM.', &
'  OLD      Scalar of the same type and kind as ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_fetch_add', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*], old', &
'      call atomic_add(atom[1], this_image(), old)', &
'   end program demo_atomic_fetch_add', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_add(3), iso_fortran_env(3),', &
'   atomic_fetch_and(3), atomic_fetch_or(3), atomic_fetch_xor(3)', &
'', &
'']

shortname="atomic_fetch_add"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('24','atomic_fetch_and')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_fetch_and(3f) - [FORTRAN:INTRINSIC:ATOMIC:BIT MANIPULATION] Atomic', &
'   bitwise AND operation with prior fetch', &
'', &
'SYNTAX', &
'   call atomic_fetch_and(atom, value, old [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_fetch_and(atom, value, old) atomically stores the value of', &
'   ATOM in OLD and defines ATOM with the bitwise AND between the', &
'   values of ATOM and VALUE. When STAT is present and the', &
'   invocation was successful, it is assigned the value 0. If it is', &
'   present and the invocation has failed, it is assigned a positive', &
'   value; in particular, for a coindexed ATOM, if the remote image has', &
'   stopped, it is assigned the value of iso_fortran_env''s', &
'   stat_stopped_image and if the remote image has failed, the value', &
'   stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer type with', &
'           atomic_int_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind is', &
'           different, the value is converted to the kind of ATOM.', &
'  OLD      Scalar of the same type and kind as ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_fetch_and', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*], old', &
'      call atomic_fetch_and (atom[1], int(b''10100011101''), old)', &
'   end program demo_atomic_fetch_and', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_and(3), iso_fortran_env(3),', &
'   atomic_fetch_add(3), atomic_fetch_or(3), atomic_fetch_xor(3)', &
'', &
'']

shortname="atomic_fetch_and"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('25','atomic_fetch_or')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_fetch_or(3f) - [FORTRAN:INTRINSIC:ATOMIC:BIT MANIPULATION] Atomic', &
'   bitwise OR operation with prior fetch', &
'', &
'SYNTAX', &
'   call atomic_fetch_or(atom, value, old [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_fetch_or(atom, value, old) atomically stores the value of ATOM', &
'   in OLD and defines ATOM with the bitwise OR between the values of ATOM', &
'   and VALUE. When STAT is present and the invocation was successful,', &
'   it is assigned the value 0. If it is present and the invocation', &
'   has failed, it is assigned a positive value; in particular, for a', &
'   coindexed ATOM, if the remote image has stopped, it is assigned the', &
'   value of iso_fortran_env''s stat_stopped_image and if the remote image', &
'   has failed, the value stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer', &
'           type with atomic_int_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind', &
'           is different, the value is converted to the kind of ATOM.', &
'  OLD      Scalar of the same type and kind as ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_fetch_or', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*], old', &
'      call atomic_fetch_or(atom[1], int(b''10100011101''), old)', &
'   end program demo_atomic_fetch_or', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_or(3), iso_fortran_env(3),', &
'   atomic_fetch_add(3), atomic_fetch_and(3), atomic_fetch_xor(3)', &
'']

shortname="atomic_fetch_or"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('26','atomic_fetch_xor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_fetch_xor(3f) - [FORTRAN:INTRINSIC:ATOMIC:BIT MANIPULATION] Atomic', &
'   bitwise XOR operation with prior fetch', &
'', &
'SYNTAX', &
'   call atomic_fetch_xor (atom, value, old [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_fetch_xor(atom, value, old) atomically stores the value of', &
'   ATOM in OLD and defines ATOM with the bitwise XOR between the', &
'   values of ATOM and VALUE. When STAT is present and the', &
'   invocation was successful, it is assigned the value 0. If it is', &
'   present and the invocation has failed, it is assigned a positive', &
'   value; in particular, for a coindexed ATOM, if the remote image has', &
'   stopped, it is assigned the value of iso_fortran_env''s', &
'   stat_stopped_image and if the remote image has failed, the value', &
'   stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer', &
'           type with atomic_int_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind', &
'           is different, the value is converted to the kind of ATOM.', &
'  OLD      Scalar of the same type and kind as ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_fetch_xor', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*], old', &
'      call atomic_fetch_xor (atom[1], int(b''10100011101''), old)', &
'   end program demo_atomic_fetch_xor', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_xor(3), iso_fortran_env(3),', &
'   atomic_fetch_add(3), atomic_fetch_and(3), atomic_fetch_or(3)', &
'', &
'']

shortname="atomic_fetch_xor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('27','atomic_or')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_or(3f) - [FORTRAN:INTRINSIC:ATOMIC:BIT MANIPULATION] Atomic bitwise', &
'   OR operation', &
'', &
'SYNTAX', &
'   call atomic_or(atom, value [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_or(atom, value) atomically defines ATOM with the bitwise', &
'   OR between the values of ATOM and VALUE. When STAT is present', &
'   and the invocation was successful, it is assigned the value 0. If it', &
'   is present and the invocation has failed, it is assigned a positive', &
'   value; in particular, for a coindexed ATOM, if the remote image has', &
'   stopped, it is assigned the value of iso_fortran_env''s', &
'   stat_stopped_image and if the remote image has failed, the value', &
'   stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer', &
'           type with atomic_int_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind', &
'           is different, the value is converted to the kind of ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_or', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*]', &
'      call atomic_or(atom[1], int(b''10100011101''))', &
'   end program demo_atomic_or', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_fetch_or(3), iso_fortran_env(3),', &
'   atomic_add(3), atomic_or(3), atomic_xor(3)', &
'', &
'']

shortname="atomic_or"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('28','atomic_ref')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_ref(3f) - [FORTRAN:INTRINSIC:ATOMIC] Obtaining the value of a', &
'   variable atomically', &
'', &
'SYNTAX', &
'   call atomic_ref(value, atom [, stat])', &
'', &
'DESCRIPTION', &
'', &
'   atomic_ref(VALUE, ATOM ) atomically assigns the value of the variable', &
'   ATOM to VALUE. When STAT is present and the invocation was', &
'   successful, it is assigned the value 0. If it is present and the', &
'   invocation has failed, it is assigned a positive value; in particular,', &
'   for a coindexed ATOM, if the remote image has stopped, it is', &
'   assigned the value of iso_fortran_env''s STAT_STOPPED_IMAGE and if', &
'   the remote image has failed, the value STAT_FAILED_IMAGE.', &
'', &
'ARGUMENTS', &
'  VALUE    Scalar of the same type as ATOM. If the kind', &
'           is different, the value is converted to the kind of ATOM.', &
'  ATOM     Scalar coarray or coindexed variable of either integer type', &
'           with atomic_int_kind kind or logical type with', &
'           atomic_logical_kind kind.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_ref', &
'   use iso_fortran_env', &
'   implicit none', &
'   logical(atomic_logical_kind) :: atom[*]', &
'   logical :: val', &
'      call atomic_ref( val, atom[1] )', &
'      ! ...', &
'      call atomic_ref( val, atom[1] )', &
'      if (val) then', &
'         print *, "Obtained"', &
'      endif', &
'   end program demo_atomic_ref', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later; with STAT, [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_cas(3), iso_fortran_env(3),', &
'   atomic_fetch_add(3), atomic_fetch_and(3), atomic_fetch_or(3),', &
'   atomic_fetch_xor(3)', &
'', &
'']

shortname="atomic_ref"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('29','atomic_xor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   atomic_xor(3f) - [FORTRAN:INTRINSIC:ATOMIC:BIT MANIPULATION] Atomic bitwise', &
'   OR operation', &
'', &
'SYNTAX', &
'   call atomic_xor(atom, value [, stat])', &
'', &
'DESCRIPTION', &
'   atomic_xor(atom, value) atomically defines ATOM with the bitwise', &
'   XOR between the values of ATOM and VALUE. When STAT is present', &
'   and the invocation was successful, it is assigned the value 0. If it', &
'   is present and the invocation has failed, it is assigned a positive', &
'   value; in particular, for a coindexed ATOM, if the remote image has', &
'   stopped, it is assigned the value of iso_fortran_env''s', &
'   stat_stopped_image and if the remote image has failed, the value', &
'   stat_failed_image.', &
'', &
'ARGUMENTS', &
'  ATOM     Scalar coarray or coindexed variable of integer type with', &
'           atomic_int_kind kind.', &
'  VALUE    Scalar of the same type as ATOM. If the kind is', &
'           different, the value is converted to the kind of ATOM.', &
'  STAT     (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_atomic_xor', &
'   use iso_fortran_env', &
'   implicit none', &
'   integer(atomic_int_kind) :: atom[*]', &
'      call atomic_xor(atom[1], int(b''10100011101''))', &
'   end program demo_atomic_xor', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Atomic subroutine', &
'', &
'SEE ALSO', &
'   atomic_define(3), atomic_fetch_xor(3), iso_fortran_env(3),', &
'   atomic_add(3), atomic_or(3), atomic_xor(3)', &
'']

shortname="atomic_xor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('30','backspace')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   BACKSPACE(7f) - [FORTRAN:FILE_POSITIONING] - backspace one record on', &
'   specified I/O unit', &
'', &
'SYNOPSIS', &
'   BACKSPACE file-unit-number', &
'   BACKSPACE([UNIT=]file-unit-number][,IOMSG=iomsg-variable]', &
'              [,IOSTAT=scalar-int-variable][,ERR=label])', &
'', &
'DESCRIPTION', &
'   Execution of a BACKSPACE statement causes the file connected to the', &
'   specified unit to be positioned before the current record if there', &
'   is a current record, or before the preceding record if there is no', &
'   current record. If the file is at its initial point, the position of', &
'   the file is not changed.', &
'', &
'   BACKSPACE works with typical files being accessed sequentially but', &
'   does not work with standard input from a terminal and other similar', &
'   file types.', &
'', &
'   It is usually used when a program has partially read a line and', &
'   then wants to go back and reread the line using the information', &
'   from the first read. Backspacing can be very inefficient (Note that', &
'   positioning to specific records in direct access files is typically', &
'   much faster). It is usually better to read the line into a CHARACTER', &
'   variable and then read from the variable multiple times using an', &
'   internal READ, or parsing the string.', &
'', &
'   Backspacing over records written using list-directed or namelist', &
'   formatting is prohibited. It will usually work, but since the compiler', &
'   is free to write list-directed or namelist output on a varying number', &
'   of lines it is not supported, as it is not certain what data is on', &
'   which line.', &
'', &
'   Backspacing a file that is connected but does not exist is prohibited.', &
'', &
'   If a BACKSPACE statement causes the implicit writing of an endfile', &
'   record, the file is positioned before the record that precedes the', &
'   endfile record.', &
'', &
'   If the preceding record is an endfile record, the file is positioned', &
'   before the endfile record.', &
'', &
'OPTIONS', &
'   UNIT     unit number of file to backspace one line on.', &
'            A unit open for direct access or unformatted access cannot', &
'            be referenced by a BACKSPACE.', &
'   IOSTAT   a compiler-specific number that indicates an error occurred', &
'            if non-zero.', &
'   IOMSG    a message describing error IOSTAT if IOSTAT is not zero.', &
'   ERR      a label number to jump to if an error occurs', &
'', &
'EXAMPLE', &
'  An example of a BACKSPACE statement is:', &
'', &
'   program demo_backspace', &
'   implicit none', &
'   character(len=256) :: line', &
'   character(len=256) :: mssge', &
'   integer            :: i', &
'   integer            :: ios', &
'      open(10,file=''dem_backspace.txt'') ! open a file', &
'      do i=1,100                         ! write lines to it', &
'         write(10,''(a,i0)'') ''line '',i', &
'      enddo', &
'      do i=1,10                          ! back up several lines', &
'         backspace(10, iostat=ios,iomsg=mssge)', &
'         if(ios.ne.0)then', &
'                 write(*,''(*(a))'') ''*dem_backspace* ERROR:'',mssge', &
'         endif', &
'      enddo', &
'      read(10,''(a)'')line', &
'      write(*,*)''back at a previous record !''', &
'      write(*,''(1x,a)'')line', &
'      !! writing new line will truncate file to current record position', &
'      close(10,status=''delete'')', &
'   end program demo_backspace', &
'', &
'  Expected Results:', &
'', &
'    back at a previous record !', &
'    line 91', &
'', &
' JSU', &
'']

shortname="backspace"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('31','bessel_j0')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bessel_j0(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Bessel function of the first', &
'   kind of order 0', &
'', &
'SYNTAX', &
'   result = bessel_j0(x)', &
'', &
'DESCRIPTION', &
'   bessel_j0(x) computes the [[Bessel function]] of the first kind of', &
'   order 0 of X.', &
'', &
'ARGUMENTS', &
'  X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL and lies in the', &
'   range -0.4027 <= Bessel(0,x) <= 1.', &
'   It has the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_besj0', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 0.0_real64', &
'      x = bessel_j0(x)', &
'   end program demo_besj0', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   bessel_j1(3), bessel_jn(3), bessel_y0(3), bessel_y1(3), bessel_yn(3)', &
'']

shortname="bessel_j0"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('32','bessel_j1')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bessel_j1(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Bessel function of the first', &
'   kind of order 1', &
'', &
'SYNTAX', &
'   result = bessel_j1(x)', &
'', &
'DESCRIPTION', &
'   bessel_j1(x) computes the [[Bessel function]] of the first kind of', &
'   order 1 of X.', &
'', &
'ARGUMENTS', &
'  X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL and lies in the', &
'   range -0.5818 <= Bessel(0,x) <= 0.5818 . It has the same', &
'   kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_besj1', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 1.0_real64', &
'      x = bessel_j1(x)', &
'   end program demo_besj1', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   bessel_j0(3), bessel_jn(3), bessel_y0(3), bessel_y1(3), bessel_yn(3)', &
'', &
'']

shortname="bessel_j1"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('33','bessel_jn')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bessel_jn(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Bessel function of the first', &
'   kind', &
'', &
'SYNTAX', &
'* result = bessel_jn(n, x)', &
'* result = bessel_jn(n1, n2, x)', &
'', &
'DESCRIPTION', &
'   bessel_jn(n, x) computes the [[Bessel function]] of the first kind', &
'   of order N of X. If N and X are arrays, their ranks and shapes', &
'   shall conform.', &
'', &
'   bessel_jn(n1, n2, x) returns an array with the [[Bessel function|Bessel', &
'   functions]] of the first kind of the orders N1 to N2.', &
'', &
'ARGUMENTS', &
'   N     Shall be a scalar or an array of type INTEGER.', &
'   N1    Shall be a non-negative scalar of type INTEGER.', &
'   N2    Shall be a non-negative scalar of type INTEGER.', &
'   X     Shall be a scalar or an array of type REAL.', &
'         For bessel_jn(n1, n2, x) it shall be scalar.', &
'', &
'RETURN VALUE', &
'   The return value is a scalar of type REAL. It has the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_besjn', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'   implicit none', &
'   real(kind=real64) :: x = 1.0_real64', &
'     x = bessel_jn(5,x)', &
'   end program demo_besjn', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]],', &
'   except for the transformational variant bessel_jn(n1, n2, x).', &
'', &
'SEE ALSO', &
'   bessel_j0(3), bessel_j1(3), bessel_y0(3), bessel_y1(3), bessel_yn(3)', &
'']

shortname="bessel_jn"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('34','bessel_y0')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bessel_y0(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Bessel function of the', &
'   second kind of order 0', &
'', &
'SYNTAX', &
'   result = bessel_y0(x)', &
'', &
'DESCRIPTION', &
'   bessel_y0(x) computes the [[Bessel function]] of the second kind of', &
'   order 0 of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL. It has the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_besy0', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'   implicit none', &
'     real(kind=real64) :: x = 0.0_real64', &
'     x = bessel_y0(x)', &
'   end program demo_besy0', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   bessel_j0(3), bessel_j1(3), bessel_jn(3), bessel_y1(3), bessel_yn(3)', &
'', &
'']

shortname="bessel_y0"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('35','bessel_y1')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bessel_y1(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Bessel function of the', &
'   second kind of order 1', &
'', &
'SYNTAX', &
'   result = bessel_y1(x)', &
'', &
'DESCRIPTION', &
'   bessel_y1(x) computes the [[Bessel function]] of the second kind of', &
'   order 1 of X.', &
'', &
'ARGUMENTS', &
'  X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is REAL. It has the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_besy1', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'   implicit none', &
'     real(kind=real64) :: x = 1.0_real64', &
'     x = bessel_y1(x)', &
'   end program demo_besy1', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   bessel_j0(3), bessel_j1(3), bessel_jn(3), bessel_y0(3), bessel_yn(3)', &
'']

shortname="bessel_y1"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('36','bessel_yn')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bessel_yn(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Bessel function of the', &
'   second kind', &
'', &
'SYNTAX', &
'* result = bessel_yn(n, x)', &
'* result = bessel_yn(n1, n2, x)', &
'', &
'DESCRIPTION', &
'   bessel_yn(n, x) computes the [[Bessel function]] of the second kind', &
'   of order N of X. If N and X are arrays, their ranks and', &
'   shapes shall conform.', &
'', &
'   bessel_yn(n1, n2, x) returns an array with the', &
'   [[Bessel function|Bessel functions]] of the first kind of the orders', &
'   N1 to N2.', &
'', &
'ARGUMENTS', &
'  N     Shall be a scalar or an array of type INTEGER.', &
'  N1    Shall be a non-negative scalar of type INTEGER.', &
'  N2    Shall be a non-negative scalar of type INTEGER.', &
'  X     Shall be a scalar or an array of type REAL;', &
'        for bessel_yn(n1, n2, x) it shall be scalar.', &
'', &
'RETURN VALUE', &
'   The return value is REAL. It has the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_besyn', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'   implicit none', &
'   real(kind=real64) :: x = 1.0_real64', &
'     x = bessel_yn(5,x)', &
'   end program demo_besyn', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]],', &
'   except for the transformational function bessel_yn(n1, n2, x)', &
'', &
'SEE ALSO', &
'   bessel_j0(3), bessel_j1(3), bessel_jn(3), bessel_y0(3), bessel_y1(3)', &
'']

shortname="bessel_yn"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('37','bge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bge(3f) - [FORTRAN:INTRINSIC:BIT COMPARE] Bitwise greater than or equal to', &
'', &
'DESCRIPTION', &
'   Determines whether an integer is a bitwise greater than or equal to another.', &
'', &
'SYNTAX', &
'   result = bge(i, j)', &
'', &
'ARGUMENTS', &
'   I    Shall be of INTEGER type.', &
'   J    Shall be of INTEGER type, and of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The return value is of type LOGICAL and of the default kind.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   bgt(3), ble(3), blt(3)', &
'', &
'']

shortname="bge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('38','bgt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bgt(3f) - [FORTRAN:INTRINSIC:BIT COMPARE] Bitwise greater than', &
'', &
'SYNTAX', &
'   result = bgt(i, j)', &
'', &
'DESCRIPTION', &
'   Determines whether an integer is bitwise greater than another.', &
'', &
'ARGUMENTS', &
'  I    Shall be of INTEGER type or a BOZ literal constant.', &
'  J    Shall be of INTEGER type, and of the same kind as I;', &
'       or a BOZ literal constant.', &
'', &
'RETURN VALUE', &
'  The return value is of type LOGICAL and of the default kind.', &
'  The result is true if the sequence of bits represented by I is greater', &
'  than the sequence of bits', &
'  represented by J, otherwise the result is false.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   bge(3), ble(3), blt(3)', &
'']

shortname="bgt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('39','bit_size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   bit_size(3f) - [FORTRAN:INTRINSIC:BIT INQUIRY] Bit size inquiry function', &
'', &
'SYNTAX', &
'   result = bit_size(i)', &
'', &
'DESCRIPTION', &
'   bit_size(i) returns the number of bits (integer precision plus sign bit)', &
'   represented by the type of I.', &
'', &
'ARGUMENTS', &
'   I    The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER of the same type as I.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_bit_size', &
'   use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'   implicit none', &
'   integer(kind=int64)          :: answer', &
'   integer                      :: ilen', &
'      write(*,''(i0)'')bit_size(bit_size(0_int8))', &
'      write(*,''(i0)'')bit_size(bit_size(0_int16))', &
'      write(*,''(i0)'')bit_size(bit_size(0_int32))', &
'      write(*,''(i0)'')bit_size(bit_size(0_int64))', &
'      answer=0_int64', &
'      ilen=999', &
'      ! notice use of INT(3f)', &
'      ilen=min(ilen,int(bit_size(answer)))', &
'      ! arguments to MIN(3f) would be of different TYPES', &
'      !ilen=min(ilen,bit_size(answer))', &
'      write(*,''(i0)'')ilen', &
'   end program demo_bit_size', &
'', &
'  Expected output:', &
'', &
'    > 8', &
'    > 16', &
'    > 32', &
'    > 64', &
'    > 64', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="bit_size"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('40','ble')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ble(3f) - [FORTRAN:INTRINSIC:BIT COMPARE] Bitwise less than or equal to', &
'', &
'DESCRIPTION', &
'   Determines whether an integer is bitwise less than or equal to another.', &
'', &
'SYNTAX', &
'   result = ble(i, j)', &
'', &
'ARGUMENTS', &
'   I    Shall be of INTEGER type.', &
'   J    Shall be of INTEGER type, and of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The return value is of type LOGICAL and of the default kind.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   bge(3), bgt(3), blt(3)', &
'', &
'']

shortname="ble"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('41','block')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   block(7f) - [FORTRAN:EXECUTION CONTROL] block construct', &
'   (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'  [block-construct-name:] BLOCK', &
'', &
'   [specification-part]', &
'', &
'   ENDBLOCK [block-construct-name]', &
'', &
'DESCRIPTION', &
'   The BLOCK construct is an executable construct which may contain', &
'   declarations, and may be exited using the EXIT statement.', &
'', &
'   The specification-part of a BLOCK construct cannot contain a COMMON,', &
'   EQUIVALENCE, IMPLICIT, INTENT, NAMELIST, or OPTIONAL statement.', &
'', &
'   A SAVE of a common-block-name is not allowed in a BLOCK construct.', &
'', &
'   Except for the ASYNCHRONOUS and VOLATILE statements, specifications', &
'   in a BLOCK construct declare construct entities whose scope is that', &
'   of the BLOCK construct.', &
'', &
'EXAMPLES', &
'  Sample programs:', &
'', &
'   program demo_block', &
'   implicit none', &
'   integer,parameter :: arr1(*)=[1,2,3,4,5,6,7]', &
'   integer,parameter :: arr2(*)=[0,1,2,3,4,5,6,7]', &
'     call showme(arr1)', &
'     call showme(arr2)', &
'     contains', &
'   subroutine showme(a)', &
'   integer,intent(in) :: a(:)', &
'   integer :: i=-100', &
'   integer :: tan', &
'     tan=20 ! intentionally cause a conflict with intrinsic', &
'     TESTFORZERO: block', &
'        integer :: I  ! local block variable', &
'       intrinsic :: tan  ! can use the TAN intrinsic in the block', &
'        do i=1,size(a)', &
'           if(a(i).eq.0) then', &
'              write(*,*)''found zero at index'',i', &
'             exit TESTFORZERO', &
'          endif', &
'        enddo', &
'        write(*,*)''Never found a zero, tried '',i-1,'' times''', &
'        return', &
'      endblock TESTFORZERO', &
'      ! note the variable I in the block is local to the block', &
'      write(*,*)''this is the variable in the main scope of the program, I='',i', &
'   end subroutine showme', &
'', &
'   end program demo_block', &
'', &
'  Results:', &
'', &
'    Never found a zero, tried 7 times', &
'    found zero at index 1', &
'    this is the variable in the main scope of the program, I= -100', &
'', &
' JSU', &
'']

shortname="block"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('42','blt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   blt(3f) - [FORTRAN:INTRINSIC:BIT COMPARE] Bitwise less than', &
'', &
'SYNTAX', &
'   result = blt(i, j)', &
'', &
'DESCRIPTION', &
'   Determines whether an integer is bitwise less than another.', &
'', &
'ARGUMENTS', &
'  I    Shall be of INTEGER type.', &
'  J    Shall be of INTEGER type, and of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The return value is of type LOGICAL and of the default kind.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   bge(3), bgt(3), ble(3)', &
'']

shortname="blt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('43','btest')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   btest(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bit test function', &
'', &
'SYNTAX', &
'   result = btest(i, pos)', &
'', &
'DESCRIPTION', &
'   btest(i,pos) returns logical .true. if the bit at POS', &
'   in I is set.', &
'', &
'ARGUMENTS', &
'   I    The type shall be INTEGER.', &
'   POS  The type shall be INTEGER. A value of zero refers to the least', &
'        significant bit.', &
'', &
'RETURN VALUE', &
'   The return value is of type LOGICAL', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_btest', &
'    implicit none', &
'    integer :: i = 32768 + 1024 + 64', &
'    integer :: pos', &
'    logical :: bool', &
'        do pos=0,16', &
'            bool = btest(i, pos)', &
'            print *, pos, bool', &
'        end do', &
'    end program demo_btest', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ibclr(3), ibits(3), ibset(3), iand(3), ior(3), ieor(3),', &
'   mvbits(3)', &
'']

shortname="btest"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('44','c_associated')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   c_associated(3f) - [FORTRAN:INTRINSIC:ISO_C_BINDING] Status of a C pointer', &
'', &
'SYNTAX', &
'    result = c_associated(c_prt_1[, c_ptr_2])', &
'', &
'DESCRIPTION', &
'   c_associated(c_prt_1[, c_ptr_2]) determines the status of the C pointer', &
'   c_ptr_1 or if c_ptr_1 is associated with the target c_ptr_2.', &
'', &
'ARGUMENTS', &
'   c_ptr_1    Scalar of the type c_ptr or c_funptr.', &
'   c_ptr_2    (Optional) Scalar of the same type as c_ptr_1.', &
'', &
'RETURN VALUE', &
'   The return value is of type LOGICAL; it is .false. if either', &
'   c_ptr_1 is a C NULL pointer or if c_ptr1 and c_ptr_2', &
'   point to different addresses.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_c_associated', &
'    contains', &
'    subroutine association_test(a,b)', &
'    use iso_c_binding, only: c_associated, c_loc, c_ptr', &
'    implicit none', &
'    real, pointer :: a', &
'    type(c_ptr) :: b', &
'       if(c_associated(b, c_loc(a))) &', &
'          stop ''b and a do not point to same target''', &
'    end subroutine association_test', &
'    end program demo_c_associated', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   c_loc(3), c_funloc(3), iso_c_binding(3)', &
'', &
'']

shortname="c_associated"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('45','ceiling')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ceiling(3f) - [FORTRAN:INTRINSIC:NUMERIC] Integer ceiling function', &
'', &
'SYNTAX', &
'   result = ceiling(a [, kind])', &
'', &
'DESCRIPTION', &
'   ceiling(a) returns the least integer greater than or equal to A.', &
'', &
'ARGUMENTS', &
'   A      The type shall be REAL.', &
'   KIND   (Optional) An INTEGER initialization', &
'          expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type integer(kind) if KIND is present', &
'   and a default-kind INTEGER otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_ceiling', &
'    implicit none', &
'    real :: x = 63.29', &
'    real :: y = -63.59', &
'       print *, ceiling(x) ! returns 64', &
'       print *, ceiling(y) ! returns -63', &
'    end program demo_ceiling', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   floor(3), nint(3)', &
'']

shortname="ceiling"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('46','c_f_pointer')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   c_f_pointer(3f) - [FORTRAN:INTRINSIC:ISO_C_BINDING] Convert C into', &
'   Fortran pointer', &
'', &
'SYNTAX', &
'    call c_f_pointer(cptr, fptr[, shape])', &
'', &
'DESCRIPTION', &
'   c_f_pointer(cptr, fptr[, shape]) Assign the target, the C pointer,', &
'   CPTR to the Fortran pointer FPTR and specify its', &
'   shape.', &
'', &
'ARGUMENTS', &
'   CPTR    scalar of the type c_ptr. It is', &
'           intent(in).', &
'   FPTR    pointer interoperable with CPTR. It is', &
'           intent(out).', &
'   SHAPE   (Optional) Rank-one array of type INTEGER', &
'           with intent(in). It shall be present', &
'           if and only if FPTR is an array. The size', &
'           must be equal to the rank of FPTR.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_c_f_pointer', &
'    use iso_c_binding', &
'    implicit none', &
'    interface', &
'       subroutine my_routine(p) bind(c,name=''myC_func'')', &
'          import :: c_ptr', &
'          type(c_ptr), intent(out) :: p', &
'       end subroutine', &
'    end interface', &
'    type(c_ptr) :: cptr', &
'    real,pointer :: a(:)', &
'       call my_routine(cptr)', &
'       call c_f_pointer(cptr, a, [12])', &
'    end program demo_c_f_pointer', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   c_loc(3), c_f_procpointer(3), iso_c_binding(3)', &
'']

shortname="c_f_pointer"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('47','c_f_procpointer')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   c_f_procpointer(3f) - [FORTRAN:INTRINSIC:ISO_C_BINDING] Convert C into', &
'   Fortran procedure pointer', &
'', &
'SYNTAX', &
'   call c_f_procpointer(cptr, fptr)', &
'', &
'DESCRIPTION', &
'   c_f_procpointer(cptr, fptr) assigns the target of the C', &
'   function pointer CPTR to the Fortran procedure pointer', &
'   FPTR.', &
'', &
'ARGUMENTS', &
'  CPTR    scalar of the type c_funptr.', &
'          It is intent(in).', &
'  FPTR    procedure pointer interoperable with CPTR.', &
'          It is intent(out).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_c_f_procpointer', &
'    use iso_c_binding', &
'    implicit none', &
'    abstract interface', &
'       function func(a)', &
'       import :: c_float', &
'       real(c_float), intent(in) :: a', &
'       real(c_float) :: func', &
'       end function', &
'    end interface', &
'    interface', &
'       function getIterFunc() bind(c,name="getIterFunc")', &
'       import :: c_funptr', &
'       type(c_funptr) :: getIterFunc', &
'       end function', &
'    end interface', &
'    type(c_funptr) :: cfunptr', &
'    procedure(func), pointer :: myFunc', &
'       cfunptr = getIterFunc()', &
'       call c_f_procpointer(cfunptr, myFunc)', &
'    end program demo_c_f_procpointer', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   c_loc(3), c_f_pointer(3), iso_c_binding(3)', &
'']

shortname="c_f_procpointer"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('48','c_funloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   c_funloc(3f) - [FORTRAN:INTRINSIC:ISO_C_BINDING] Obtain the C address', &
'   of a procedure', &
'', &
'SYNTAX', &
'    result = c_funloc(x)', &
'', &
'DESCRIPTION', &
'   c_funloc(x) determines the C address of the argument.', &
'', &
'ARGUMENTS', &
'   X    Interoperable function or pointer to such function.', &
'', &
'RETURN VALUE', &
'   The return value is of type c_funptr and contains the C address', &
'   of the argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    ! program demo_c_funloc and module', &
'    module x', &
'    use iso_c_binding', &
'    implicit none', &
'    contains', &
'    subroutine sub(a) bind(c)', &
'    real(c_float) :: a', &
'       a = sqrt(a)+5.0', &
'    end subroutine sub', &
'    end module x', &
'    !', &
'    program demo_c_funloc', &
'    use iso_c_binding', &
'    use x', &
'    implicit none', &
'    interface', &
'       subroutine my_routine(p) bind(c,name=''myC_func'')', &
'         import :: c_funptr', &
'         type(c_funptr), intent(in) :: p', &
'       end subroutine', &
'    end interface', &
'       call my_routine(c_funloc(sub))', &
'    !', &
'    end program demo_c_funloc', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   c_associated(3), c_loc(3),', &
'   c_f_pointer(3), c_f_procpointer(3),', &
'   iso_c_binding(3)', &
'']

shortname="c_funloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('49','char')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   char(3f) - [FORTRAN:INTRINSIC:CHARACTER] Character conversion function', &
'', &
'SYNTAX', &
'   result = char(i [, kind])', &
'', &
'DESCRIPTION', &
'   char(i [, kind]) returns the character represented by the integer I.', &
'   ARGUMENTS', &
'', &
'   I      The type shall be INTEGER.', &
'   KIND   (Optional) An INTEGER initialization', &
'          expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type character(1)', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_char', &
'    implicit none', &
'    integer :: i = 74', &
'    character(1) :: c', &
'        c = char(i)', &
'        print *, i, c ! returns ''J''', &
'    end program demo_char', &
'', &
'NOTE', &
'   See [[ichar]] for a discussion of converting between numerical values', &
'   and formatted string representations.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   [[achar]], [[iachar]], [[ichar]]', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="char"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('50','c_loc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   c_loc(3f) - [FORTRAN:INTRINSIC:ISO_C_BINDING] Obtain the C address of', &
'   an object', &
'', &
'SYNTAX', &
'   result = c_loc(x)', &
'', &
'DESCRIPTION', &
'   c_loc(x) determines the C address of the argument.', &
'', &
'ARGUMENTS', &
'   X    Shall have either the POINTER or TARGET attribute. It', &
'        shall not be a coindexed object. It shall either be a variable', &
'        with interoperable type and kind type parameters, or be a scalar,', &
'        nonpolymorphic variable with no length type parameters.', &
'', &
'RETURN VALUE', &
'   The return value is of type c_ptr and contains the C address', &
'   of the argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   subroutine association_test(a,b)', &
'   use iso_c_binding, only: c_associated, c_loc, c_ptr', &
'   implicit none', &
'   real, pointer :: a', &
'   type(c_ptr) :: b', &
'     if(c_associated(b, c_loc(a))) &', &
'        stop ''b and a do not point to same target''', &
'   end subroutine association_test', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   c_associated(3), c_funloc(3),', &
'   c_f_pointer(3), c_f_procpointer(3),', &
'   iso_c_binding(3)', &
'']

shortname="c_loc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('51','close')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   CLOSE(7f) - [FORTRAN:IO] terminate the connection of a specified unit', &
'   to an external file.', &
'   ', &
'SYNOPSIS', &
'   CLOSE ( [UNIT= ] file-unit-number,', &
'', &
'    [IOSTAT= scalar-int-variable,]', &
'    [IOMSG= iomsg-variable,] ', &
'    [ERR= label,] ', &
'    [STATUS= scalar-default-char-expr]', &
'   )', &
'', &
'DESCRIPTION', &
'', &
'   The CLOSE statement is used to terminate the connection of a specified', &
'   unit to an external file.', &
'', &
'   Execution of a CLOSE statement for a unit may occur in any program', &
'   unit of a program and need not occur in the same program unit as the', &
'   execution of an OPEN statement referring to that unit.', &
'', &
'   Execution of a CLOSE statement performs a wait operation for any', &
'   pending asynchronous data transfer operations for the specified unit.', &
'', &
'   Execution of a CLOSE statement specifying a unit that does not exist or', &
'   has no file connected to it is permitted and affects no file or unit.', &
'', &
'   After a unit has been disconnected by execution of a CLOSE statement,', &
'   it may be connected again within the same program, either to the same', &
'   file or to a different file. After a named file has been disconnected', &
'   by execution of a CLOSE statement, it may be connected again within', &
'   the same program, either to the same unit or to a different unit,', &
'   provided that the file still exists.', &
'', &
'   The input/output statements are the OPEN, CLOSE, READ, WRITE, PRINT,', &
'   BACKSPACE, ENDFILE, REWIND, FLUSH, WAIT, and INQUIRE statements.', &
'', &
'   OPEN, CLOSE, BACKSPACE, ENDFILE, and REWIND statements shall not be', &
'   executed while a parent data transfer statement is active.', &
'', &
'   A pure subprogram shall not contain a print-stmt, open-stmt,', &
'   close-stmt, backspace-stmt, endfile-stmt, rewind-stmt, flush-stmt,', &
'   wait-stmt, or inquire-stmt.', &
'', &
'   The READ statement is a data transfer input statement. The', &
'   WRITE statement and the PRINT statement are data transfer output', &
'   statements. The OPEN statement and the CLOSE statement are file', &
'   connection statements. The INQUIRE statement is a file inquiry', &
'   statement. The BACKSPACE, ENDFILE, and REWIND statements are file', &
'   positioning statements.', &
'', &
'   All input/output statements may refer to files that exist. An INQUIRE,', &
'   OPEN, CLOSE, WRITE, PRINT, REWIND, FLUSH, or ENDFILE statement', &
'   also may refer to a file that does not exist. Execution of a WRITE,', &
'   PRINT, or ENDFILE statement referring to a preconnected file that', &
'   does not exist creates the file. This file is a different file from', &
'   one preconnected on any other image.', &
'', &
'   AT PROGRAM TERMINATION', &
'   During the completion step of termination of execution of a program,', &
'   all units that are connected are closed. Each unit is closed with', &
'   status KEEP unless the file status prior to termination of execution', &
'   was SCRATCH, in which case the unit is closed with status DELETE.', &
'', &
'     The effect is as though a CLOSE statement without a STATUS=', &
'     specifier were executed on each connected unit.', &
'', &
'OPTIONS', &
'  No specifier shall appear more than once in a given close-spec-list.', &
'', &
'  UNIT=file-unit-number            A file-unit-number shall be specified', &
'                                   in a close-spec-list; if the', &
'                                   optional characters UNIT= are omitted,', &
'                                   the file-unit-number shall be the', &
'                                   first item in the close-spec-list.', &
'  IOSTAT=scalar-int-variable       0 means no error occurred', &
'  IOMSG=iomsg-variable             Character variable to hold message', &
'                                   if an error occurred.', &
'  ERR=label                        The label used in the ERR= specifier', &
'                                   shall be the statement label of a', &
'                                   branch target statement that appears', &
'                                   in the same scoping unit as the', &
'                                   CLOSE statement.', &
'  STATUS=scalar-default-char-expr  The expression has a limited list of', &
'                                   character values. Any trailing blanks', &
'                                   are ignored. The value specified is', &
'                                   without regard to case.', &
'', &
'                                   The scalar-default-char-expr shall', &
'                                   evaluate to KEEP or DELETE. The', &
'                                   STATUS= specifier determines the', &
'                                   disposition of the file that is', &
'                                   connected to the specified unit. KEEP', &
'                                   shall not be specified for a file', &
'                                   whose status prior to execution of a', &
'                                   CLOSE statement is SCRATCH. If KEEP', &
'                                   is specified for a file that exists,', &
'                                   the file continues to exist after the', &
'                                   execution of a CLOSE statement. If KEEP', &
'                                   is specified for a file that does not', &
'                                   exist, the file will not exist after', &
'                                   the execution of a CLOSE statement. If', &
'                                   DELETE is specified, the file will', &
'                                   not exist after the execution of a', &
'                                   CLOSE statement. If this specifier', &
'                                   is omitted, the default value is', &
'                                   KEEP, unless the file status prior', &
'                                   to execution of the CLOSE statement', &
'                                   is SCRATCH, in which case the default', &
'                                   value is DELETE.', &
'', &
'EXAMPLE', &
'  sample program:', &
'', &
'   program demo_close', &
'   implicit none', &
'   character(len=256) :: message', &
'   integer            :: ios', &
'      open (10, file=''employee.names'', action=''read'', iostat=ios,iomsg=message)', &
'      if (ios < 0) then', &
'         ! perform end-of-file processing on the file connected to unit 10.', &
'', &
'         close (10, status=''keep'',iostat=ios,iomsg=message)', &
'         if(ios.ne.0)then', &
'            write(*,''(*(a))'')''*demo_close* close error: '',trim(message)', &
'            stop 1', &
'         endif', &
'      elseif (ios > 0) then', &
'         ! perform error processing on open', &
'         write(*,''(*(a))'')''*demo_close* open error: '',trim(message)', &
'         stop 2', &
'      endif', &
'   end program demo_close', &
'']

shortname="close"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('52','cmplx')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   cmplx(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Complex conversion function', &
'', &
'SYNTAX', &
'   result = cmplx(x [, y [, kind]])', &
'', &
'DESCRIPTION', &
'   To convert numeric variables to complex, use the CMPLX function.', &
'   Constants can be used to define a complex variable using the syntax', &
'', &
'      z8 = (1.2345678901234567d0, 1.2345678901234567d0)', &
'', &
'   but this will not work for variables. You must use the CMPLX function.', &
'', &
'   CMPLX(X [, Y [, KIND]]) returns a complex number where X is converted', &
'   to the real component. If X is complex then Y must not be present.', &
'   If Y is present it is converted to the imaginary component. If Y is', &
'   not present then the imaginary component is set to 0.0.', &
'', &
'   CMPLX AND DOUBLE PRECISION', &
'', &
'   The Fortran 90 language defines CMPLX() as always returning a result', &
'   that is type COMPLEX(KIND=KIND(0.0)).', &
'', &
'   This means `CMPLX(D1,D2)'', where `D1'' and `D2'' are DOUBLEPRECISION,', &
'   is treated as:', &
'', &
'      CMPLX(SNGL(D1), SNGL(D2))', &
'', &
'   DOUBLEPRECISION complex numbers require specifying a precision.', &
'', &
'   It was necessary for Fortran 90 to specify this behavior for', &
'   DOUBLEPRECISION arguments, since that is the behavior mandated by', &
'   FORTRAN 77.', &
'', &
'   So Fortran 90 extends the CMPLX() intrinsic by adding an extra argument', &
'   used to specify the desired kind of complex result.', &
'', &
'      integer,parameter :: dp=kind(0.0d0)', &
'      complex(kind=dp) :: z8', &
'      !', &
'      ! NO: result is just the precision of default REAL values', &
'      !     because KIND parameter is not specified', &
'      !', &
'      ! note this was stored with default real precision', &
'      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)', &
'      print *, ''NO, Z8='',z8,real(z8),aimag(z8)', &
'      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)', &
'      ! again, note components are just REAL', &
'      print *, ''NO, Z8='',z8,real(z8),aimag(z8)', &
'      !', &
'      ! YES', &
'      !', &
'      ! kind= makes it work', &
'      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)', &
'      print *, ''YES, Z8='',z8,real(z8),aimag(z8)', &
'', &
'   F2018 COMPONENT SYNTAX', &
'   The real and imaginary parts of a complex entity can be accessed', &
'   independently with a component-like syntax in f2018:', &
'', &
'   A complex-part-designator is', &
'', &
'      designator % RE', &
'      or', &
'      designator % IM.', &
'', &
'   Where the designator is of complex type.', &
'', &
'   So designator%RE designates the real part of a complex value,', &
'   designator%IM designates the imaginary part of complex value. The', &
'   type of a complex-part-designator is REAL, and its kind and shape', &
'   are those of the designator.', &
'', &
'   The following are examples of complex part designators:', &
'', &
'       impedance%re           !-- Same value as REAL(impedance)', &
'       fft%im                 !-- Same value as AIMAG(fft)', &
'       x%im = 0.0             !-- Sets the imaginary part of X to zero', &
'', &
'ARGUMENTS', &
'   X       The type may be INTEGER, REAL, or COMPLEX.', &
'   Y       (Optional; only allowed if X is not COMPLEX.).', &
'           May be INTEGER or REAL.', &
'   KIND    (Optional) An INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of COMPLEX type, with a kind equal to', &
'   KIND if it is specified. If KIND is not specified, the', &
'   result is of the default COMPLEX kind, regardless of the kinds of', &
'   X and Y.', &
'', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_aimag', &
'   implicit none', &
'   integer,parameter :: dp=kind(0.0d0)', &
'   complex          :: z4', &
'   complex(kind=dp) :: z8', &
'      z4 = cmplx(1.23456789, 1.23456789)', &
'      print *, ''Z4='',z4', &
'      ! using kind=dp makes it keep DOUBLEPRECISION precision', &
'      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)', &
'      print *, ''Z8='',z8', &
'      ! NOTE:', &
'      ! The following is intuitive and works without calling cmplx(3f)', &
'      ! but does not work for variables just constants', &
'      z8 = (1.2345678901234567d0 , 1.2345678901234567d0 )', &
'      print *, ''Z8 defined with constants='',z8', &
'   end program demo_aimag', &
'', &
'  Typical Results:', &
'', &
'    Z4= (1.23456788,1.23456788)', &
'    Z8= (1.2345678901234567,1.2345678901234567)', &
'    Z8 defined with constants= (1.2345678901234567,1.2345678901234567)', &
'', &
'SEE ALSO', &
'   o aimag(3f) -  Imaginary part of complex number', &
'   o cmplx(3f) -  Complex conversion function', &
'   o conjg(3f) -  Complex conjugate function', &
'   o real(3f)  -  Convert to real type', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="cmplx"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('53','co_broadcast')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_broadcast(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Copy a value to all images', &
'   the current set of images', &
'', &
'SYNTAX', &
'   call co_broadcast(A, SOURCE_IMAGE [, STAT, ERRMSG])', &
'', &
'DESCRIPTION', &
'   co_broadcast copies the value of argument A on the image with', &
'   image index source_image to all images in the current team. A', &
'   becomes defined as if by intrinsic assignment. If the execution was', &
'   successful and STAT is present, it is assigned the value zero. If the', &
'   execution failed, STAT gets assigned a nonzero value and, if present,', &
'   ERRMSG gets assigned a value describing the occurred error.', &
'', &
'ARGUMENTS', &
'   A    intent(inout) argument; shall have the same dynamic type and', &
'        type parameters on all images of the current team. If it is an array,', &
'        it shall have the same shape on all images.', &
'', &
'   SOURCE_IMAGE   a scalar integer expression. It shall have the', &
'                  same the same value on all images and refer to an', &
'                  image of the current team.', &
'', &
'   STAT           (optional) a scalar integer variable', &
'', &
'   ERRMSG         (optional) a scalar character variable', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_co_broadcast', &
'   implicit none', &
'   integer :: val(3)', &
'      if (this_image() == 1) then', &
'        val = [1, 5, 3]', &
'      endif', &
'      call co_broadcast (val, source_image=1)', &
'      print *, this_image(), ":", val', &
'   end program demo_co_broadcast', &
'', &
'SEE ALSO', &
'   co_max(3), co_min(3), co_sum(3), co_reduce(3)', &
'', &
'']

shortname="co_broadcast"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('54','co_lbound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_lbound(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Lower codimension bounds of', &
'   an array', &
'', &
'SYNTAX', &
'   result = CO_LBOUND(coarray [, dim [, kind]])', &
'', &
'DESCRIPTION', &
'   Returns the lower bounds of a coarray, or a single lower cobound', &
'   along the DIM codimension.', &
'', &
'ARGUMENTS', &
'  ARRAY    Shall be an coarray, of any type.', &
'  DIM      (Optional) Shall be a scalar INTEGER.', &
'  KIND     (Optional) An INTEGER initialization expression', &
'  indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If KIND is', &
'   absent, the return value is of default integer kind. If DIM is', &
'   absent, the result is an array of the lower cobounds of COARRAY. If', &
'   DIM is present, the result is a scalar corresponding to the lower', &
'   cobound of the array along that codimension.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Inquiry function', &
'', &
'SEE ALSO', &
'   co_ubound(3), lbound(3)', &
'']

shortname="co_lbound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('55','co_max')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_max(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Maximal value on the current', &
'   set of images', &
'', &
'SYNTAX', &
'   call co_max(a [, result_image, stat, errmsg])', &
'', &
'DESCRIPTION', &
'   co_max determines element-wise the maximal value of A on all', &
'   images of the current team. If result_image is present, the maximum', &
'   values are returned in A on the specified image only and the value', &
'   of A on the other images become undefined. If result_image is not', &
'   present, the value is returned on all images. If the execution was', &
'   successful and STAT is present, it is assigned the value zero. If', &
'   the execution failed, STAT gets assigned a nonzero value and, if', &
'   present, ERRMSG gets assigned a value describing the occurred error.', &
'', &
'ARGUMENTS', &
'  A              shall be an integer, real or character variable, which', &
'                 has the same type and type parameters on all images of', &
'                 the team.', &
'  result_image   (optional) a scalar integer expression; if present,', &
'                 it shall have the same the same value on all images', &
'                 and refer to an image of the current team.', &
'  STAT           (optional) a scalar integer variable', &
'  ERRMSG         (optional) a scalar character variable', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_co_max', &
'   implicit none', &
'   integer :: val', &
'      val = this_image()', &
'      call co_max(val, result_image=1)', &
'      if (this_image() == 1) then', &
'        write(*,*) "Maximal value", val  ! prints num_images()', &
'      endif', &
'   end program demo_co_max', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   [[Collective subroutine]]', &
'', &
'SEE ALSO', &
'   co_min(3), co_sum(3), co_reduce(3), co_broadcast(3)', &
'']

shortname="co_max"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('56','co_min')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_min(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Minimal value on the current set', &
'   of images', &
'', &
'SYNTAX', &
'   call co_min(a [, result_image, stat, errmsg])', &
'', &
'DESCRIPTION', &
'', &
'   co_min determines element-wise the minimal value of A on all', &
'   images of the current team. If result_image is present, the minimal', &
'   values are returned in A on the specified image only and the value', &
'   of A on the other images become undefined. If result_image is not', &
'   present, the value is returned on all images. If the execution was', &
'   successful and STAT is present, it is assigned the value zero. If', &
'   the execution failed, STAT gets assigned a nonzero value and, if', &
'   present, ERRMSG gets assigned a value describing the occurred error.', &
'', &
'ARGUMENTS', &
'  A              shall be an integer, real or character variable, which', &
'                 has the same type and type parameters on all images of', &
'                 the team.', &
'  result_image   (optional) a scalar integer expression; if present,', &
'                 it shall have the same the same value on all images', &
'                 and refer to an image of the current team.', &
'  STAT           (optional) a scalar integer variable', &
'  ERRMSG         (optional) a scalar character variable', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_co_min', &
'   implicit none', &
'   integer :: val', &
'      val = this_image()', &
'      call co_min(val, result_image=1)', &
'      if (this_image() == 1) then', &
'        write(*,*) "Minimal value", val  ! prints 1', &
'      endif', &
'   end program demo_co_min', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   [[Collective subroutine]]', &
'', &
'SEE ALSO', &
'   co_max(3), co_sum(3), co_reduce(3), co_broadcast(3)', &
'']

shortname="co_min"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('57','command_argument_count')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   command_argument_count(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] Get', &
'   number of command line arguments', &
'', &
'SYNTAX', &
'   result = command_argument_count()', &
'', &
'     integer :: result', &
'', &
'DESCRIPTION', &
'   command_argument_count returns the number of arguments passed on the', &
'   command line when the containing program was invoked.', &
'', &
'ARGUMENTS', &
'   None', &
'', &
'RETURN VALUE', &
'   RESULT  The return value is of type default integer.', &
'           It is the number of arguments passed on the command line', &
'           when the program was invoked.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_command_argument_count', &
'   implicit none', &
'   integer :: count', &
'       count = command_argument_count()', &
'       print *, count', &
'   end program demo_command_argument_count', &
'', &
'  Sample output:', &
'', &
'   # the command verb does not count', &
'   ./test_command_argument_count', &
'       0', &
'   # quoted strings may count as one argument', &
'   ./test_command_argument_count count arguments', &
'       2', &
'   ./test_command_argument_count ''count arguments''', &
'       1', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   get_command(3), get_command_argument(3)', &
' JSU', &
'']

shortname="command_argument_count"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('58','compiler_options')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   compiler_options(3f) - [FORTRAN:INTRINSIC:COMPILER INQUIRY] Options passed', &
'   to the compiler', &
'', &
'SYNTAX', &
'   str = compiler_options()', &
'', &
'DESCRIPTION', &
'   compiler_options returns a string with the options used for', &
'   compiling.', &
'', &
'ARGUMENTS', &
'   None.', &
'', &
'RETURN VALUE', &
'   The return value is a default-kind string with system-dependent', &
'   length. It contains the compiler flags used to compile the file,', &
'   which called the compiler_options intrinsic.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_compiler_version', &
'   use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options', &
'   implicit none', &
'      print ''(4a)'', &', &
'         ''This file was compiled by '', &', &
'         compiler_version(),           &', &
'         '' using the options '',        &', &
'         compiler_options()', &
'   end program demo_compiler_version', &
'', &
'  Example results:', &
'', &
'   This file was compiled by GCC version 5.4.0 using the options', &
'   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall', &
'   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan', &
'   -fno-range-check -frecord-marker=4', &
'   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'', &
'STANDARD', &
'   [[Fortran 2008]]', &
'', &
'CLASS', &
'   Inquiry function of the module [[iso_fortran_env]]', &
'', &
'SEE ALSO', &
'   compiler_version(3), iso_fortran_env(7)', &
'']

shortname="compiler_options"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('59','compiler_version')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   compiler_version(3f) - [FORTRAN:INTRINSIC:COMPILER INQUIRY] Compiler version', &
'   string', &
'', &
'SYNTAX', &
'   str = compiler_version()', &
'', &
'DESCRIPTION', &
'   compiler_version(3f) returns a string containing the name and version', &
'   of the compiler.', &
'', &
'ARGUMENTS', &
'   None.', &
'', &
'RETURN VALUE', &
'   The return value is a default-kind string with system-dependent', &
'   length. It contains the name of the compiler and its version number.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_compiler_version', &
'   use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options', &
'   implicit none', &
'      print ''(4a)'', &', &
'         ''This file was compiled by '', &', &
'         compiler_version(),           &', &
'         '' using the options '',        &', &
'         compiler_options()', &
'   end program demo_compiler_version', &
'', &
'  Example results:', &
'', &
'   This file was compiled by GCC version 5.4.0 using the options', &
'   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall', &
'   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan', &
'   -fno-range-check -frecord-marker=4', &
'   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN', &
'', &
'STANDARD', &
'   [[Fortran 2008]]', &
'', &
'CLASS', &
'   Inquiry function of the module [[iso_fortran_env]]', &
'', &
'SEE ALSO', &
'   compiler_options(3), iso_fortran_env(7)', &
'']

shortname="compiler_version"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('60','conjg')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   conjg(3f) - [FORTRAN:INTRINSIC:NUMERIC] Complex conjugate function', &
'', &
'SYNTAX', &
'   z = conjg(z)', &
'', &
'DESCRIPTION', &
'   conjg(z) returns the conjugate of Z. If Z is (x, y)', &
'   then the result is (x, -y)', &
'', &
'ARGUMENTS', &
'   Z    The type shall be COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of type COMPLEX.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_conjg', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    complex :: z = (2.0, 3.0)', &
'    complex(kind=real64) :: dz = (&', &
'    &  1.2345678901234567_real64, &', &
'    & -1.2345678901234567_real64)', &
'        z= conjg(z)', &
'        print *, z', &
'        dz = conjg(dz)', &
'        print *, dz', &
'    end program demo_conjg', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="conjg"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('61','continue')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   CONTINUE(7f) - [FORTRAN:EXECUTION_CONTROL] execution of a CONTINUE statement', &
'   has no effect', &
'', &
'SYNOPSIS', &
'   NNNNN continue', &
'', &
'DESCRIPTION', &
'  Execution of a CONTINUE statement has no effect. A CONTINUE statement', &
'  is most often used as a numerically labeled line that is used as a', &
'  target for transfer control statements such as GOTO.', &
'', &
'  It is generally very confusing to have executable statements on labeled', &
'  lines; a CONTINUE statement eliminates the ambiguities that arise in', &
'  jumping to an executable line. Preferably no target of a transfer', &
'  should be an executable statement, but should be a statement like', &
'  ENDDO or CONTINUE.', &
'', &
'  CONTINUE was very frequently used as the end of a DO loop; ENDDO is', &
'  now the proper way to end a DO loop.', &
'', &
'EXAMPLE', &
' Sample program:', &
'', &
'   program oldstyle', &
'   integer :: i,j', &
'         i=10', &
'         j=5', &
'         if(i.lt.5)goto 100', &
'         j=3', &
'   100   write(*,*)''J='',j', &
'   end', &
'', &
'   program demo_continue', &
'   implicit none', &
'   integer :: i,j', &
'         i=10', &
'         j=5', &
'         if(i.lt.5)goto 100', &
'         j=3', &
'   100   continue', &
'         write(*,*)''J='',j', &
'   end program demo_continue', &
'', &
' JSU', &
'']

shortname="continue"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('62','co_reduce')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_reduce(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Reduction of values on the', &
'   current set of images', &
'', &
'SYNTAX', &
'   call co_reduce(a, operator, [, result_image, stat, errmsg])', &
'', &
'DESCRIPTION', &
'   co_reduce determines element-wise the reduction of the value of A', &
'   on all images of the current team. The pure function passed as', &
'   OPERATOR is used to pairwise reduce the values of A by passing', &
'   either the value of A of different images or the result values of', &
'   such a reduction as argument. If A is an array, the deduction is', &
'   done element wise. If result_image is present, the result values are', &
'   returned in A on the specified image only and the value of A on', &
'   the other images become undefined. If result_image is not present,', &
'   the value is returned on all images. If the execution was successful', &
'   and STAT is present, it is assigned the value zero. If the', &
'   execution failed, STAT gets assigned a nonzero value and, if', &
'   present, ERRMSG gets assigned a value describing the occurred error.', &
'', &
'ARGUMENTS', &
'  A  - is an intent(inout) argument and shall be nonpolymorphic. If', &
'  it is allocatable, it shall be allocated; if it is a pointer, it', &
'  shall be associated. A shall have the same type and type', &
'  parameters on all images of the team; if it is an array, it shall', &
'  have the same shape on all images.', &
'', &
'  OPERATOR  - pure function with two scalar nonallocatable arguments,', &
'  which shall be nonpolymorphic and have the same type and type', &
'  parameters as A. The function shall return a nonallocatable', &
'  scalar of the same type and type parameters as A. The function', &
'  shall be the same on all images and with regards to the arguments', &
'  mathematically commutative and associative. Note that OPERATOR', &
'  may not be an elemental function, unless it is an intrinsic function.', &
'  result_image  - (optional) a scalar integer expression; if present,', &
'  it shall have the same the same value on all images and refer to an', &
'  image of the current team.', &
'', &
'  STAT  - (optional) a scalar integer variable', &
'', &
'  ERRMSG  - (optional) a scalar character variable', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_co_reduce', &
'   implicit none', &
'     integer :: val', &
'     val = this_image()', &
'     call co_reduce(val, result_image=1, operator=myprod)', &
'        if (this_image() == 1) then', &
'          write(*,*) "Product value", val  ! prints num_images() factorial', &
'        endif', &
'   contains', &
'     pure function myprod(a, b)', &
'       integer, value :: a, b', &
'       integer :: myprod', &
'       myprod = a * b', &
'     end function myprod', &
'   end program demo_co_reduce', &
'', &
'NOTE', &
'   While the rules permit in principle an intrinsic function, none of the', &
'   intrinsics in the standard fulfill the criteria of having a specific', &
'   function, which takes two arguments of the same type and returning', &
'   that type as result.', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   [[Collective subroutine]]', &
'', &
'SEE ALSO', &
'   co_min(3), co_max(3), co_sum(3), co_broadcast(3)', &
'', &
'']

shortname="co_reduce"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('63','cos')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   cos(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Cosine function', &
'', &
'SYNTAX', &
'   result = cos(x)', &
'', &
'DESCRIPTION', &
'   cos(x) computes the cosine of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X. The real part', &
'   of the result is in radians. If X is of the type REAL, the return', &
'   value lies in the range -1 <= cos(x) <= 1.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_cos', &
'   implicit none', &
'   doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0', &
'   write(*,*)''COS(0.0)='',cos(0.0)', &
'   write(*,*)''COS(PI)='',cos(PI)', &
'   write(*,*)''COS(PI/2.0d0)='',cos(PI/2.0d0),'' EPSILON='',epsilon(PI)', &
'   write(*,*)''COS(2*PI)='',cos(2*PI)', &
'   write(*,*)''COS(-2*PI)='',cos(-2*PI)', &
'   write(*,*)''COS(-2000*PI)='',cos(-2000*PI)', &
'   write(*,*)''COS(3000*PI)='',cos(3000*PI)', &
'   end program demo_cos', &
'', &
'  Expected output:', &
'', &
'   COS(0.0)=        1.00000000', &
'   COS(PI)=        -1.0000000000000000', &
'   COS(PI/2.0d0)=   6.1232339957367660E-017', &
'   EPSILON=         2.2204460492503131E-016', &
'   COS(2*PI)=       1.0000000000000000', &
'   COS(-2*PI)=      1.0000000000000000', &
'   COS(-2000*PI)=   1.0000000000000000', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   acos(3), sin(3), tan(3)', &
'']

shortname="cos"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('64','cosh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   cosh(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Hyperbolic cosine', &
'   function', &
'', &
'SYNTAX', &
'   x = cosh(x)', &
'', &
'DESCRIPTION', &
'   cosh(x) computes the hyperbolic cosine of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as X. If X is complex, the', &
'   imaginary part of the result is in radians. If X is REAL, the', &
'   return value has a lower bound of one, cosh(x) >= 1.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_cosh', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128', &
'   implicit none', &
'   real(kind=real64) :: x = 1.0_real64', &
'      x = cosh(x)', &
'   end program demo_cosh', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, for a complex argument [[Fortran 2008]] or later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   Inverse function: acosh(3)', &
'', &
'']

shortname="cosh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('65','co_sum')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_sum(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Sum of values on the current set', &
'   of images', &
'', &
'SYNTAX', &
'   call co_sum(a [, result_image, stat, errmsg])', &
'', &
'DESCRIPTION', &
'   co_sum sums up the values of each element of A on all images of', &
'   the current team. If result_image is present, the summed-up values', &
'   are returned in A on the specified image only and the value of A', &
'   on the other images become undefined. If result_image is not', &
'   present, the value is returned on all images. If the execution was', &
'   successful and STAT is present, it is assigned the value zero. If', &
'   the execution failed, STAT gets assigned a nonzero value and, if', &
'   present, ERRMSG gets assigned a value describing the occurred error.', &
'', &
'ARGUMENTS', &
'  A    shall be an integer, real or complex variable, which has the same', &
'       type and type parameters on all images of the team.', &
'', &
'  result_image   (optional) a scalar integer expression; if present,', &
'                 it shall have the same the same value on all images', &
'                 and refer to an image of the current team.', &
'', &
'  STAT     (optional) a scalar integer variable', &
'', &
'  ERRMSG   (optional) a scalar character variable', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_co_sum', &
'   implicit none', &
'   integer :: val', &
'      val = this_image()', &
'      call co_sum(val, result_image=1)', &
'      if (this_image() == 1) then', &
'         ! prints (n**2 + n)/2, with n = num_images()', &
'         write(*,*) "The sum is ", val', &
'      endif', &
'   end program demo_co_sum', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   [[Collective subroutine]]', &
'', &
'SEE ALSO', &
'   co_max(3), co_min(3), co_reduce(3), co_broadcast(3)', &
'', &
'']

shortname="co_sum"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('66','co_ubound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   co_ubound(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Upper codimension bounds of', &
'   an array', &
'', &
'SYNTAX', &
'   result = CO_UBOUND(coarray [, dim [, kind]])', &
'', &
'DESCRIPTION', &
'   Returns the upper cobounds of a coarray, or a single upper cobound', &
'   along the DIM codimension.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an coarray, of any type.', &
'   DIM      (Optional) Shall be a scalar INTEGER.', &
'   KIND     (Optional) An INTEGER initialization expression indicating', &
'            the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If KIND is', &
'   absent, the return value is of default integer kind. If DIM is', &
'   absent, the result is an array of the lower cobounds of COARRAY. If', &
'   DIM is present, the result is a scalar corresponding to the lower', &
'   cobound of the array along that codimension.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Inquiry function', &
'', &
'SEE ALSO', &
'   co_lbound(3), lbound(3), ubound(3)', &
'']

shortname="co_ubound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('67','count')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   count(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] Count function', &
'', &
'SYNTAX', &
'   result = count(mask [, dim, kind])', &
'', &
'DESCRIPTION', &
'   Counts the number of .true. elements in a logical MASK,', &
'   or, if the DIM argument is supplied, counts the number of', &
'   elements along each row of the array in the DIM direction.', &
'   If the array has zero size, or all of the elements of MASK are', &
'   false, then the result is 0.', &
'', &
'ARGUMENTS', &
'   MASK    The type shall be LOGICAL.', &
'   DIM     (Optional) The type shall be INTEGER.', &
'   KIND    (Optional) An INTEGER initialization', &
'           expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'   If DIM is present, the result is an array with a rank one less', &
'   than the rank of ARRAY, and a size corresponding to the shape', &
'   of ARRAY with the DIM dimension removed.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_count', &
'   implicit none', &
'   integer, dimension(2,3) :: a, b', &
'   logical, dimension(2,3) :: mymask', &
'      a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])', &
'      b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])', &
'      print ''(3i3)'', a(1,:)', &
'      print ''(3i3)'', a(2,:)', &
'      print *', &
'      print ''(3i3)'', b(1,:)', &
'      print ''(3i3)'', b(2,:)', &
'      print *', &
'      mymask = a.ne.b', &
'      print ''(3l3)'', mymask(1,:)', &
'      print ''(3l3)'', mymask(2,:)', &
'      print *', &
'      print ''(3i3)'', count(mymask)', &
'      print *', &
'      print ''(3i3)'', count(mymask, 1)', &
'      print *', &
'      print ''(3i3)'', count(mymask, 2)', &
'   end program demo_count', &
'', &
'   Expected Results:', &
'', &
'     > 1  3  5', &
'     > 2  4  6', &
'     >', &
'     > 0  3  5', &
'     > 7  4  8', &
'     >', &
'     > T  F  F', &
'     > T  F  T', &
'     >', &
'     > 3', &
'     >', &
'     > 2  0  1', &
'     >', &
'     > 1  2', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="count"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('68','cpu_time')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   cpu_time(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] return CPU processor', &
'   time in seconds', &
'', &
'SYNTAX', &
'   call cpu_time(time)', &
'', &
'      real,intent(out) :: time', &
'', &
'DESCRIPTION', &
'   Returns a REAL value representing the elapsed CPU time in seconds. This', &
'   is useful for testing segments of code to determine execution time.', &
'', &
'   The exact definition of time is left imprecise because of the', &
'   variability in what different processors are able to provide.', &
'', &
'   If no time source is available, TIME is set to a negative value.', &
'', &
'   Note that TIME may contain a system dependent, arbitrary offset', &
'   and may not start with 0.0. For cpu_time the absolute value is', &
'   meaningless. Only differences between subsequent calls, as shown in', &
'   the example below, should be used.', &
'', &
'   A processor for which a single result is inadequate (for example,', &
'   a parallel processor) might choose to provide an additional version', &
'   for which time is an array.', &
'', &
'RETURN VALUE', &
'   TIME  The type shall be REAL with intent(out).', &
'         It is assigned a processor-dependent approximation to the', &
'         processor time in seconds. If the processor cannot return', &
'         a meaningful time, a processor-dependent negative value', &
'         is returned.  The start time is left imprecise because the', &
'         purpose is to time sections of code, as in the example.', &
'         This might or might not include system overhead time.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_cpu_time', &
'    implicit none', &
'    real :: start, finish', &
'       !', &
'       call cpu_time(start)', &
'       ! put code to test here', &
'       call cpu_time(finish)', &
'       !', &
'       ! writes processor time taken by the piece of code.', &
'       print ''("Processor Time = ",f6.3," seconds.")'',finish-start', &
'    end program demo_cpu_time', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   system_clock(3), date_and_time(3)', &
'', &
' JSU', &
'']

shortname="cpu_time"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('69','cshift')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   cshift(3f) - [FORTRAN:INTRINSIC:TRANSFORMATIONAL FUNCTION] Circular shift', &
'   elements of an array', &
'', &
'SYNTAX', &
'   result = cshift(array, shift [, dim])', &
'', &
'DESCRIPTION', &
'   cshift(array, shift [, dim]) performs a circular shift on elements', &
'   of ARRAY along the dimension of DIM. If DIM is omitted it is taken', &
'   to be 1. DIM is a scalar of type INTEGER in the range of 1 <= DIM <=', &
'   n, where "n" is the rank of ARRAY. If the rank of ARRAY is one,', &
'   then all elements of ARRAY are shifted by SHIFT places. If rank is', &
'   greater than one, then all complete rank one sections of ARRAY along', &
'   the given dimension are shifted. Elements shifted out one end of each', &
'   rank one section are shifted back in the other end.', &
'', &
'ARGUMENTS', &
'   ARRAY   Shall be an array of any type.', &
'   SHIFT   The type shall be INTEGER.', &
'   DIM     The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   Returns an array of same type and rank as the ARRAY argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_cshift', &
'    implicit none', &
'    integer, dimension(3,3) :: a', &
'        a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'        a = cshift(a, SHIFT=[1, 2, -1], DIM=2)', &
'        print *', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'    end program demo_cshift', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="cshift"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('70','c_sizeof')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   c_sizeof(3f) - [FORTRAN:INTRINSIC:ISO_C_BINDING] Size in bytes of an', &
'   expression', &
'', &
'SYNTAX', &
'    n = c_sizeof(x)', &
'', &
'DESCRIPTION', &
'   c_sizeof(x) calculates the number of bytes of storage the', &
'   expression X occupies.', &
'', &
'ARGUMENTS', &
'   X    The argument shall be an interoperable data entity.', &
'', &
'RETURN VALUE', &
'   The return value is of type integer and of the system-dependent kind', &
'   c_size_t (from the [[iso_c_binding]] module). Its value is the', &
'   number of bytes occupied by the argument. If the argument has the', &
'   POINTER attribute, the number of bytes of the storage area pointed', &
'   to is returned. If the argument is of a derived type with POINTER', &
'   or ALLOCATABLE components, the return value does not account for', &
'   the sizes of the data pointed to by these components.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_c_sizeof', &
'   use iso_c_binding', &
'   implicit none', &
'   real(c_float) :: r, s(5)', &
'      print *, (c_sizeof(s)/c_sizeof(r) == 5)', &
'   end program demo_c_sizeof', &
'', &
'  The example will print .true. unless you are using a platform', &
'  where default REAL variables are unusually padded.', &
'', &
'STANDARD', &
'   [[Fortran 2008]]', &
'', &
'CLASS', &
'   Intrinsic function', &
'', &
'SEE ALSO', &
'   storage_size(3)', &
'', &
'']

shortname="c_sizeof"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('71','date_and_time')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   date_and_time(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] gets current time', &
'', &
'SYNTAX', &
'    subroutine date_and_time([date, time, zone, values])', &
'', &
'     character(len=8),intent(out) :: date', &
'     character(len=10),intent(out) :: time', &
'     character(len=5),intent(out) :: zone', &
'     integer,intent(out) :: values', &
'', &
'DESCRIPTION', &
'   DATE_AND_TIME(date, time, zone, values) gets the corresponding date', &
'   and time information from the real-time system clock.', &
'', &
'   Unavailable time and date parameters return blanks.', &
'', &
'ARGUMENTS', &
'   DATE      The type shall be character(len=8) or larger,', &
'             and of default kind.  DATE has form ccyymmdd.', &
'   TIME      The type shall be character(len=10) or', &
'             larger, and of default kind.  TIME has form hhmmss.sss.', &
'   ZONE      The type shall be character(len=5) or larger,', &
'             and of default kind.  ZONE has form (+-)hhmm, representing', &
'             the difference', &
'             with respect to Coordinated Universal Time (UTC). ', &
'   VALUES    The type shall be integer(8). VALUES provide the following:', &
'', &
'               * value(1): - The year', &
'               * value(2): - The month', &
'               * value(3): - The day of the month', &
'               * value(4): - Time difference with UTC in minutes', &
'               * value(5): - The hour of the day', &
'               * value(6): - The minutes of the hour', &
'               * value(7): - The seconds of the minute', &
'               * value(8): - The milliseconds of the second', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_time_and_date', &
'    implicit none', &
'    character(len=8)     :: date', &
'    character(len=10)    :: time', &
'    character(len=5)     :: zone', &
'    integer,dimension(8) :: values', &
'        call date_and_time(date,time,zone,values)', &
'        ! using keyword arguments', &
'        call date_and_time(DATE=date,TIME=time,ZONE=zone)', &
'        call date_and_time(VALUES=values)', &
'        print ''(*(g0))'',''DATE="'',date,''" TIME="'',time,''" ZONE="'',zone,''"''', &
'       write(*,''(i5,a)'') &', &
'         & values(1),'' - The year'', &', &
'         & values(2),'' - The month'', &', &
'         & values(3),'' - The day of the month'', &', &
'         & values(4),'' - Time difference with UTC in minutes'', &', &
'         & values(5),'' - The hour of the day'', &', &
'         & values(6),'' - The minutes of the hour'', &', &
'         & values(7),'' - The seconds of the minute'', &', &
'         & values(8),'' - The milliseconds of the second''', &
'    end program demo_time_and_date', &
'', &
'  Results:', &
'', &
'   DATE="20201222" TIME="165738.779" ZONE="-0500"', &
'    2020 - The year', &
'      12 - The month', &
'      22 - The day of the month', &
'    -300 - Time difference with UTC in minutes', &
'      16 - The hour of the day', &
'      57 - The minutes of the hour', &
'      38 - The seconds of the minute', &
'     779 - The milliseconds of the second', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'SEE ALSO', &
'   cpu_time(3), system_clock(3)', &
' JSU', &
'']

shortname="date_and_time"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('72','dble')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   dble(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Double conversion function', &
'', &
'SYNTAX', &
'   result = DBLE(A)', &
'', &
'DESCRIPTION', &
'   DBLE(A) Converts A to double precision real type.', &
'', &
'ARGUMENTS', &
'   A    The type shall be INTEGER, REAL, or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of type DOUBLEPRECISION.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_dble', &
'    implicit none', &
'    real    :: x = 2.18', &
'    integer :: i = 5', &
'    complex :: z = (2.3,1.14)', &
'       print *, dble(x), dble(i), dble(z)', &
'    end program demo_dble', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   float(3), real(3)', &
'']

shortname="dble"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('73','digits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   digits(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Significant digits function', &
'', &
'SYNTAX', &
'   result = digits(x)', &
'', &
'DESCRIPTION', &
'   digits(x) returns the number of significant digits of the internal', &
'   model representation of X. For example, on a system using a 32-bit', &
'   floating point representation, a default real number would likely', &
'   return 24.', &
'', &
'ARGUMENTS', &
'   X    The type may be INTEGER or REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_digits', &
'    implicit none', &
'        integer :: i = 12345', &
'        real :: x = 3.143', &
'        doubleprecision :: y = 2.33d0', &
'        print *,''default integer:        '', digits(i)', &
'        print *,''default real:           '', digits(x)', &
'        print *,''default doubleprecision:'', digits(y)', &
'    end program demo_digits', &
'', &
'  Typical Results:', &
'', &
'    default integer:                  31', &
'    default real:                     24', &
'    default doubleprecision:          53', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="digits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('74','dim')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   dim(3f) - [FORTRAN:INTRINSIC:NUMERIC] Positive difference', &
'', &
'SYNTAX', &
'   result = DIM(X, Y)', &
'', &
'DESCRIPTION', &
'   DIM(X,Y) returns the difference X-Y if the result is positive;', &
'   otherwise returns zero.', &
'', &
'ARGUMENTS', &
'   X    The type shall be INTEGER or REAL', &
'   Y    The type shall be the same type and kind as X.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER or REAL.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_dim', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    integer :: i', &
'    real(kind=real64) :: x', &
'        i = dim(4, 15)', &
'        x = dim(4.345_real64, 2.111_real64)', &
'        print *, i', &
'        print *, x', &
'    end program demo_dim', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="dim"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('75','dot_product')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   dot_product(3f) - [FORTRAN:INTRINSIC:TRANSFORMATIONAL FUNCTION] Dot product', &
'   function', &
'', &
'SYNTAX', &
'   result = dot_product(vector_a, vector_b)', &
'', &
'DESCRIPTION', &
'   dot_product(vector_a, vector_b) computes the dot product multiplication', &
'   of two vectors vector_a and vector_b. The two vectors may be', &
'   either numeric or logical and must be arrays of rank one and of', &
'   equal size. If the vectors are INTEGER or REAL, the result is', &
'   sum(vector_a*vector_b). If the vectors are COMPLEX, the result is', &
'   sum(conjg(vector_a)*vector_b). If the vectors are LOGICAL, the result', &
'   is any(vector_a .and. vector_b).', &
'', &
'ARGUMENTS', &
'   vector_a    The type shall be numeric or LOGICAL, rank 1.', &
'   vector_b    The type shall be numeric if vector_a is of numeric type', &
'               or LOGICAL', &
'               if vector_a is of type LOGICAL. vector_b shall be a', &
'               rank-one array.', &
'', &
'RETURN VALUE', &
'   If the arguments are numeric, the return value is a scalar of numeric type,', &
'   INTEGER, REAL, or COMPLEX. If the arguments are', &
'   LOGICAL, the return value is .true. or .false..', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_dot_prod', &
'    implicit none', &
'        integer, dimension(3) :: a, b', &
'        a = [ 1, 2, 3 ]', &
'        b = [ 4, 5, 6 ]', &
'        print ''(3i3)'', a', &
'        print *', &
'        print ''(3i3)'', b', &
'        print *', &
'        print *, dot_product(a,b)', &
'    end program demo_dot_prod', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="dot_product"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('76','dprod')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   dprod(3f) - [FORTRAN:INTRINSIC:NUMERIC] Double product function', &
'', &
'SYNTAX', &
'   result = dprod(x, y)', &
'', &
'DESCRIPTION', &
'   DPROD(X,Y) produces a higher DOUBLEPRECISION product of default REAL', &
'   numbers X and Y.', &
'', &
'   The result has a value equal to a processor-dependent approximation to', &
'   the product of X and Y. It is recommended that the processor compute', &
'   the product in double precision, rather than in single precision and', &
'   then converted to double precision.', &
'', &
'   X   shall be default real.', &
'   Y   shall be default real.', &
'', &
'   The setting of compiler options specifying REAL size can affect', &
'   this function.', &
'', &
'ARGUMENTS', &
'   X   Must be of default REAL(kind=kind(0.0)) type', &
'   Y   Must have the same type and kind parameters as X', &
'', &
'RETURN VALUE', &
'   The return value is of type real(kind=kind(0.0d0)).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_dprod', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    integer,parameter :: dp=kind(0.0d0)', &
'    real :: x = 5.2', &
'    real :: y = 2.3', &
'    real(kind=dp) :: dd', &
'       dd = dprod(x,y)', &
'       print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))', &
'       ! interesting comparisons', &
'       print *, 52*23', &
'       print *, 52*23/100.0', &
'       print *, 52*23/100.0d0', &
'', &
'       !! common extension is to take doubleprecision arguments', &
'       !! and return higher precision', &
'       bigger: block', &
'       doubleprecision :: xx = 5.2d0', &
'       doubleprecision :: yy = 2.3d0', &
'       real(kind=real128) :: ddd', &
'       !ddd = dprod(xx,yy)', &
'       !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))', &
'       endblock bigger', &
'', &
'    end program demo_dprod', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="dprod"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('77','dshiftl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   dshiftl(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] combines bits of', &
'   arguments I and J', &
'', &
'SYNTAX', &
'   result = DSHIFTL(I, J, SHIFT)', &
'', &
'DESCRIPTION', &
'   DSHIFTL(I, J, SHIFT) combines bits of I and J. The rightmost', &
'   SHIFT bits of the result are the leftmost SHIFT bits of J, and', &
'   the remaining bits are the rightmost bits of I.', &
'', &
'ARGUMENTS', &
'  I       Shall be of type INTEGER.', &
'  J       Shall be of type INTEGER, and of the same kind as I.', &
'  SHIFT   Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   dshiftr(3)', &
'', &
'']

shortname="dshiftl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('78','dshiftr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   dshiftr(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] combines bits of', &
'   arguments I and J', &
'', &
'SYNTAX', &
'   result = DSHIFTL(I, J, SHIFT)', &
'', &
'DESCRIPTION', &
'   DSHIFTR(I, J, SHIFT) combines bits of I and J. The leftmost', &
'   SHIFT bits of the result are the rightmost SHIFT bits of I, and', &
'   the remaining bits are the leftmost bits of J.', &
'', &
'ARGUMENTS', &
'  I       Shall be of type INTEGER.', &
'  J       Shall be of type INTEGER, and of the same kind as I.', &
'  SHIFT   Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   dshiftl(3)', &
'']

shortname="dshiftr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('79','eoshift')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   eoshift(3f) - [FORTRAN:INTRINSIC:TRANSFORMATIONAL FUNCTION] End-off shift', &
'   elements of an array', &
'', &
'SYNTAX', &
'   result = eoshift(array, shift [, boundary, dim])', &
'', &
'DESCRIPTION', &
'   eoshift(array, shift[, boundary, dim]) performs an end-off shift on', &
'   elements of ARRAY along the dimension of DIM. If DIM is omitted it is', &
'   taken to be 1. DIM is a scalar of type INTEGER in the range of 1 <=', &
'   DIM <= n where "n" is the rank of ARRAY. If the rank of ARRAY is one,', &
'   then all elements of ARRAY are shifted by SHIFT places. If rank is', &
'   greater than one, then all complete rank one sections of ARRAY along', &
'   the given dimension are shifted. Elements shifted out one end of', &
'   each rank one section are dropped. If BOUNDARY is present then the', &
'   corresponding value of from BOUNDARY is copied back in the other', &
'   end. If BOUNDARY is not present then the following are copied in', &
'   depending on the type of ARRAY.', &
'', &
'*Array Type* - *Boundary Value*', &
'', &
'* Numeric          0 of the type and kind of ARRAY', &
'* Logical          .false.', &
'* Character(LEN)   LEN blanks', &
'', &
'ARGUMENTS', &
'   ARRAY      May be any type, not scalar.', &
'   SHIFT      The type shall be INTEGER.', &
'   BOUNDARY   Same type as ARRAY.', &
'   DIM        The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   Returns an array of same type and rank as the ARRAY argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_eoshift', &
'    implicit none', &
'        integer, dimension(3,3) :: a', &
'        a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'        a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)', &
'        print *', &
'        print ''(3i3)'', a(1,:)', &
'        print ''(3i3)'', a(2,:)', &
'        print ''(3i3)'', a(3,:)', &
'    end program demo_eoshift', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="eoshift"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('80','epsilon')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   epsilon(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Epsilon function', &
'', &
'SYNTAX', &
'   result = epsilon(x)', &
'', &
'DESCRIPTION', &
'   epsilon(x) returns a nearly negligible number relative to 1.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of same type as the argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_epsilon', &
'    implicit none', &
'        real :: x = 3.143', &
'        real(8) :: y = 2.33', &
'        print *, epsilon(x)', &
'        print *, epsilon(y)', &
'    end program demo_epsilon', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="epsilon"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('81','erf')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   erf(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Error function', &
'', &
'DESCRIPTION', &
'   erf(x) computes the error function of X, defined as', &
'   $$', &
'   \text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0**x e**{-t**2} dt.', &
'   $$', &
'', &
'SYNTAX', &
'   result = erf(x)', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL, of the same kind as', &
'   X and lies in the range -1 <= erf(x) <= 1 .', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_erf', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 0.17_real64', &
'      x = erf(x)', &
'    end program demo_erf', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="erf"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('82','erfc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   erfc(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Complementary error function', &
'', &
'SYNTAX', &
'   result = erfc(x)', &
'', &
'DESCRIPTION', &
'   erfc(x) computes the complementary error function of X, defined as', &
'   $$', &
'   1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_0**x e**{-t**2} dt.', &
'   $$', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL and of the same kind as X. It lies', &
'   in the range', &
'', &
'     0 <= ERFC(X) <= 2.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_erfc', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128', &
'   implicit none', &
'   real(kind=real64) :: x = 0.17_real64', &
'     x = erfc(x)', &
'   end program demo_erfc', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'']

shortname="erfc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('83','erfc_scaled')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   erfc_scaled(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Error function', &
'', &
'DESCRIPTION', &
'', &
'   erfc_scaled(x) computes the exponentially-scaled complementary', &
'   error function of X:', &
'', &
'   $$', &
'   e**{x**2} \frac{2}{\sqrt{\pi}} \int_{x}**{\infty} e**{-t**2} dt.', &
'   $$', &
'', &
'SYNTAX', &
'   result = erfc_scaled(x)', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL and of the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_erfc_scaled', &
'   implicit none', &
'   real(kind(0.0d0)) :: x = 0.17d0', &
'     x = erfc_scaled(x)', &
'     print *, x ! prints approx. 0.83375830214998126', &
'   end program demo_erfc_scaled', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'']

shortname="erfc_scaled"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('84','event_query')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   event_query(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Query whether a coarray', &
'   event has occurred', &
'', &
'SYNTAX', &
'   call EVENT_QUERY(EVENT, COUNT [, STAT])', &
'', &
'DESCRIPTION', &
'   EVENT_QUERY assigns the number of events to COUNT which have been', &
'   posted to the EVENT variable and not yet been removed by calling', &
'   EVENT_WAIT. When STAT is present and the invocation was', &
'   successful, it is assigned the value 0. If it is present and the', &
'   invocation has failed, it is assigned a positive value and COUNT is', &
'   assigned the value -1.', &
'', &
'ARGUMENTS', &
'  EVENT   (intent(in)) Scalar of type event_type, defined in', &
'          iso_fortran_env; shall not be coindexed.', &
'  COUNT   (intent(out))Scalar integer with at least the precision of', &
'          default integer.', &
'  STAT    (OPTIONAL) Scalar default-kind integer variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_event_query', &
'     use iso_fortran_env', &
'     implicit none', &
'     type(event_type) :: event_value_has_been_set[*]', &
'     integer :: cnt', &
'     if (this_image() == 1) then', &
'       call event_query(event_value_has_been_set, cnt)', &
'       if (cnt > 0) write(*,*) "Value has been set"', &
'     elseif (this_image() == 2) then', &
'       event post(event_value_has_been_set[1])', &
'     endif', &
'   end program demo_event_query', &
'', &
'STANDARD', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'']

shortname="event_query"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('85','execute_command_line')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   execute_command_line(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] Execute', &
'   a shell command', &
'', &
'SYNTAX', &
'   subroutine execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)', &
'', &
'    character(len=*),intent(in)  :: command', &
'    logical,intent(in),optional  :: wait', &
'    integer,intent(out),optional :: exitstat', &
'    integer,intent(out),optional :: cmdstat', &
'    character(len=*),intent(out),optional :: cmdmsg', &
'', &
'DESCRIPTION', &
'   The COMMAND argument is passed to the shell and executed. (The shell', &
'   is generally sh(1) on Unix systems, and cmd.exe on Windows.) If WAIT', &
'   is present and has the value .FALSE., the execution of the command', &
'   is asynchronous if the system supports it; otherwise, the command is', &
'   executed synchronously.', &
'', &
'   The three last arguments allow the user to get status information.', &
'   After synchronous execution, EXITSTAT contains the integer exit code', &
'   of the command, as returned by SYSTEM. CMDSTAT is set to zero if the', &
'   command line was executed (whatever its exit status was). CMDMSG is', &
'   assigned an error message if an error has occurred.', &
'', &
'   Note that the system call need not be thread-safe. It is the', &
'   responsibility of the user to ensure that the system is not called', &
'   concurrently if required.', &
'', &
'   When the command is executed synchronously, EXECUTE_COMMAND_LINE', &
'   returns after the command line has completed execution. Otherwise,', &
'   EXECUTE_COMMAND_LINE returns without waiting.', &
'', &
'ARGUMENTS', &
'   COMMAND     a default CHARACTER scalar containing the command line', &
'               to be executed. The interpretation is', &
'               programming-environment dependent.', &
'', &
'   WAIT        (Optional) a default LOGICAL scalar. If WAIT is present', &
'               with the value .false., and the processor supports', &
'               asynchronous execution of the command, the command', &
'               is executed asynchronously; otherwise it is executed', &
'               synchronously.', &
'', &
'   EXITSTAT    (Optional) an INTEGER of the default kind with', &
'               intent(INOUT).  If the command is executed synchronously,', &
'               it is assigned the value of the processor-dependent exit', &
'               status. Otherwise, the value of EXITSTAT is unchanged.', &
'', &
'   CMDSTAT     (Optional) an INTEGER of default kind with intent(INOUT).', &
'               If an error condition occurs and CMDSTAT is not present,', &
'               error termination of execution of the image is initiated.', &
'', &
'               It is assigned the value -1 if the processor does not', &
'               support command line execution, a processor-dependent', &
'               positive value if an error condition occurs, or the', &
'               value -2 if no error condition occurs but WAIT is present', &
'               with the value false and the processor does not support', &
'               asynchronous execution.Otherwise it is assigned the', &
'               value 0.', &
'', &
'   CMDMSG      (Optional) a CHARACTER scalar of the default kind.', &
'               It is an INTENT (INOUT) argument.If an error condition', &
'               occurs, it is assigned a processor-dependent explanatory', &
'               message.Otherwise, it is unchanged.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_exec', &
'   implicit none', &
'     integer :: i', &
'', &
'     call execute_command_line("external_prog.exe", exitstat=i)', &
'     print *, "Exit status of external_prog.exe was ", i', &
'', &
'     call execute_command_line("reindex_files.exe", wait=.false.)', &
'     print *, "Now reindexing files in the background"', &
'   end program demo_exec', &
'', &
'NOTE', &
'   Because this intrinsic is making a system call, it is very system', &
'   dependent. Its behavior with respect to signaling is processor', &
'   dependent. In particular, on POSIX-compliant systems, the SIGINT and', &
'   SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As', &
'   such, if the parent process is terminated, the child process might', &
'   not be terminated alongside.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'']

shortname="execute_command_line"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('86','exit')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   EXIT(7f) - [FORTRAN:EXECUTION CONTROL] statement (LICENSE:PD)', &
'', &
'SYNOPSIS', &
'   EXIT [construct-name]', &
'', &
'DESCRIPTION', &
'   The EXIT statement provides a way of terminating a loop. It can also', &
'   complete execution of other constructs.', &
'', &
'   An EXIT statement cannot occur within or refer to a DO CONCURRENT', &
'   construct even if it refers to another construct.', &
'', &
'   If a construct-name appears, the EXIT statement must be within that', &
'   construct; otherwise, it has to be within the range of at least one', &
'   do-construct.', &
'', &
'   An EXIT statement belongs to a particular construct. If a construct', &
'   name appears, the EXIT statement belongs to that construct; otherwise,', &
'   it belongs to the innermost DO construct in which it appears.', &
'', &
'   When an EXIT statement that belongs to a DO construct is executed,', &
'   it terminates the loop and any active loops contained within the', &
'   terminated loop. When an EXIT statement that belongs to a non-DO', &
'   construct is executed, it terminates any active loops contained within', &
'   that construct, and completes execution of that construct.', &
'', &
'EXAMPLES', &
'  Samples:', &
'', &
'   program demo_exit', &
'   implicit none', &
'   integer :: i, j', &
'   logical :: big', &
'      ! EXIT a simple loop', &
'      do i=1, 10', &
'         if(i .eq. 4) exit ! exit loop', &
'      enddo', &
'      write(*,*)''I='',i', &
'', &
'      ! EXIT only exits an innermost loop', &
'      do i=1,10', &
'         do j=100, 200', &
'            if(j .eq. 150) exit ! exit inner loop "j" but not "i"', &
'         enddo', &
'      enddo', &
'      write(*,*)''I='',i,'' J='',j', &
'', &
'      ! EXIT an outermost loop from inner loop by using a construct name', &
'      OUTER: do i=1,10', &
'         INNER: do j=100, 200', &
'            if(j .eq. 150) exit OUTER ! exit named loop "i"', &
'         enddo INNER', &
'      enddo OUTER', &
'      write(*,*)''I='',i,'' J='',j', &
'', &
'      ! EXIT a BLOCK not just a DO loop', &
'      MYBLOCK: block', &
'         big = .false.', &
'         do i = 1, 100', &
'           if( i==40 )then', &
'             exit MYBLOCK', &
'           endif', &
'         enddo', &
'         big = .true.', &
'      endblock MYBLOCK', &
'      write(*,*)''I='',i,'' BIG='',big', &
'   end program demo_exit', &
'', &
'  Results:', &
'', &
'    I=           4', &
'    I=          11  J=         150', &
'    I=           1  J=         150', &
'    I=          40  BIG= F', &
'', &
' JSU', &
'']

shortname="exit"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('87','exp')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   exp(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Exponential function', &
'', &
'SYNTAX', &
'   result = exp(x)', &
'', &
'DESCRIPTION', &
'   exp(x) computes the base "e" exponential of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_exp', &
'    implicit none', &
'      real :: x = 1.0', &
'      x = exp(x)', &
'    end program demo_exp', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="exp"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('88','exponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   exponent(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Exponent function', &
'', &
'SYNTAX', &
'   result = exponent(x)', &
'', &
'DESCRIPTION', &
'   exponent(x) returns the value of the exponent part of X. If X', &
'   is zero the value returned is zero.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type default INTEGER.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_exponent', &
'    implicit none', &
'      real :: x = 1.0', &
'      integer :: i', &
'      i = exponent(x)', &
'      print *, i', &
'      print *, exponent(0.0)', &
'    end program demo_exponent', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="exponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('89','extends_type_of')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   extends_type_of(3f) - [FORTRAN:INTRINSIC] determine if the dynamic type of', &
'   A is an extension of the dynamic type of MOLD.', &
'', &
'SYNOPSIS', &
'   RESULT=EXTENDS_TYPE_OF (A, MOLD)', &
'', &
'DESCRIPTION', &
'   extends_type_of(3f) is TRUE if and only if the dynamic type of A is', &
'   an extension of the dynamic type of MOLD.', &
'', &
'OPTIONS', &
'  A      shall be an object of extensible type. If it is a pointer,', &
'         it shall not have an undefined association status.', &
'  MOLD   shall be an object of extensible type. If it is a pointer,', &
'         it shall not have an undefined association status.', &
'', &
'RESULTS', &
'  RESULT   Default logical scalar.', &
'', &
'  VALUE    If MOLD is unlimited polymorphic and is either a disassociated', &
'           pointer or unallocated allocatable variable, the result is', &
'           true; otherwise if A is unlimited polymorphic and is either', &
'           a disassociated pointer or unallocated allocatable variable,', &
'           the result is false; otherwise the result is true if and only', &
'           if the dynamic type of A is an extension type of the dynamic', &
'           type of MOLD.', &
'', &
'   The dynamic type of a disassociated pointer or unallocated allocatable', &
'   variable is its declared type.', &
'', &
'EXAMPLE', &
'', &
'CLASS', &
'   Inquiry function.', &
'']

shortname="extends_type_of"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('90','findloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   findloc(3f) - [FORTRAN:INTRINSIC] Location of first element of ARRAY', &
'   identified by MASK along dimension DIM having a value', &
'', &
'SYNOPSIS', &
'  FINDLOC (ARRAY, VALUE, DIM [, MASK, KIND, BACK])', &
'   or', &
'  FINDLOC (ARRAY, VALUE [, MASK, KIND, BACK])', &
'', &
'DESCRIPTION', &
'  Location of the first element of ARRAY identified by MASK along', &
'  dimension DIM having a value equal to VALUE.', &
'', &
'', &
'  If both ARRAY and VALUE are of type logical, the comparison is performed', &
'  with the .EQV. operator; otherwise, the comparison is performed with', &
'  the == operator. If the value of the comparison is true, that element', &
'  of ARRAY matches VALUE.', &
'', &
'  If only one element matches VALUE, that element''s subscripts are', &
'  returned. Otherwise, if more than one element matches VALUE and BACK is', &
'  absent or present with the value false, the element whose subscripts are', &
'  returned is the first such element, taken in array element order. If', &
'  BACK is present with the value true, the element whose subscripts are', &
'  returned is the last such element, taken in array element order.', &
'', &
'OPTIONS', &
'  ARRAY   shall be an array of intrinsic type.', &
'  VALUE   shall be scalar and in type conformance with ARRAY, as specified', &
'          in Table 7.3 for relational intrinsic operations 7.1.5.5.2).', &
'  DIM     shall be an integer scalar with a value in the range 1 DIM n,', &
'          where n is the rank of ARRAY.', &
'          The corresponding actual argument shall not be an optional dummy', &
'          argument.', &
'  MASK    (optional) shall be of type logical and shall be conformable', &
'          with ARRAY.', &
'  KIND    (optional) shall be a scalar integer initialization expression.', &
'  BACK    (optional) shall be a logical scalar.', &
'', &
'RESULT', &
'  Result Characteristics. Integer. If KIND is present, the kind type', &
'  parameter is that specified by the value of KIND; otherwise the kind', &
'  type parameter is that of default integer type. If DIM does not appear,', &
'  the result is an array of rank one and of size equal to the rank of', &
'  ARRAY; otherwise, the result is of rank n - 1 and shape', &
'', &
'   [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ]', &
'', &
'  where', &
'', &
'   [d1 , d2 , . . . , dn ]', &
'', &
'  is the shape of ARRAY.', &
'', &
'  RESULT VALUE', &
'', &
'  Case (i):  The result of FINDLOC (ARRAY, VALUE) is a rank-one array', &
'             whose element values are the values of the subscripts', &
'             of an element of ARRAY whose value matches VALUE. If', &
'             there is such a value, the ith subscript returned lies', &
'             in the range 1 to ei , where ei is the extent of the', &
'             ith dimension of ARRAY. If no elements match VALUE or', &
'             ARRAY has size zero, all elements of the result are zero.', &
'', &
'  Case (ii):  The result of FINDLOC (ARRAY, VALUE, MASK = MASK) is a', &
'              rank-one array whose element values are the values of', &
'              the subscripts of an element of ARRAY, corresponding to', &
'              a true element of MASK, whose value matches VALUE. If', &
'              there is such a value, the ith subscript returned lies', &
'              in the range 1 to ei , where ei is the extent of the', &
'              ith dimension of ARRAY. If no elements match VALUE,', &
'              ARRAY has size zero, or every element of MASK has the', &
'              value false, all elements of the result are zero.', &
'', &
'  Case (iii):  If ARRAY has rank one, the result of', &
'', &
'                 findloc (array, value, dim=dim [, mask = mask])', &
'', &
'               is a scalar whose value is equal to that of the first element of', &
'', &
'                 findloc (array, value [, mask = mask])', &
'', &
'               Otherwise, the value of element', &
'', &
'                 (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )', &
'', &
'               of the result is equal to', &
'', &
'                 findloc (array (s1, s2, ..., sdim-1, :, sdim+1, ..., sn ), &', &
'                 value, dim=1 [, mask = mask (s1, s2, ..., sdim-1, :,', &
'                                 sdim+1 ,... , sn )]).', &
'', &
'EXAMPLE', &
'  Case (i):  The value of', &
'', &
'                  findloc ([2, 6, 4, 6,], value = 6)', &
'', &
'             is [2], and the value of', &
'', &
'                  findloc ([2, 6, 4, 6], value = 6, back = .true.)', &
'', &
'             is [4].', &
'', &
'  Case (ii):  If A', &
'              has the value', &
'', &
'                 > 0 -5  7 7', &
'                 > 3  4 -1 2', &
'                 > 1  5  6 7', &
'', &
'                and M has the value', &
'', &
'                 > T T F T', &
'                 > T T F T', &
'                 > T T F T', &
'', &
'                 FINDLOC (A, 7, MASK = M)', &
'', &
'                has the value [1, 4] and', &
'', &
'                 FINDLOC (A, 7, MASK = M, BACK = .TRUE.)', &
'', &
'                has the value [3, 4]. This is independent of the declared', &
'                lower bounds for A.', &
'', &
'  Case (iii):   The value of', &
'', &
'                 FINDLOC ([2, 6, 4], VALUE = 6, DIM = 1)', &
'', &
'                is 2. If B has the value', &
'', &
'                  > 1 2 -9', &
'                  > 2 2  6', &
'', &
'                 FINDLOC (B, VALUE = 2, DIM = 1)', &
'', &
'                has the value [2, 1, 0] and', &
'', &
'                 FINDLOC (B, VALUE = 2, DIM = 2)', &
'', &
'                has the value [2, 1]. This is independent of the declared', &
'                lower bounds for B.', &
'CLASS', &
'   Transformational function.', &
'']

shortname="findloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('91','float')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   float(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Convert integer to default real', &
'', &
'SYNTAX', &
'   result = float(a)', &
'', &
'DESCRIPTION', &
'   float(a) converts the integer A to a default real value.', &
'', &
'ARGUMENTS', &
'   A    The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type default REAL.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_float', &
'    implicit none', &
'        integer :: i = 1', &
'        if (float(i) /= 1.) stop '' FLOAT FAILED''', &
'    end program demo_float', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   dble(3), real(3)', &
'']

shortname="float"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('92','floor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   floor(3f) - [FORTRAN:INTRINSIC:NUMERIC] Integer floor function', &
'', &
'DESCRIPTION', &
'   floor(a) returns the greatest integer less than or equal to X.', &
'', &
'SYNTAX', &
'   result = floor(a [, kind])', &
'', &
'ARGUMENTS', &
'   A      The type shall be REAL.', &
'   KIND   (Optional) An INTEGER initialization', &
'          expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type integer(kind) if KIND is present', &
'   and of default-kind INTEGER otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_floor', &
'    implicit none', &
'        real :: x = 63.29', &
'        real :: y = -63.59', &
'        print *, floor(x) ! returns 63', &
'        print *, floor(y) ! returns -64', &
'    end program demo_floor', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ceiling(3), nint(3)', &
'', &
'']

shortname="floor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('93','flush')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   flush(7f) - [FORTRAN:IO] flush I/O buffers of specified files', &
'', &
'', &
'SYNOPSIS', &
'   flush file-unit-number', &
'', &
'    or', &
'', &
'   flush([UNIT=]file_unit_number,[iostat=i],[iomsg=str],[err=label_number])', &
'', &
'DESCRIPTION', &
'  The actions of FLUSH(3f) are processor dependent because the Fortran', &
'  standard does not specify the mechanism of file storage. However,', &
'  the intention is', &
'', &
'  1. The FLUSH(3f) operation should make all data written to an external', &
'     file available to other processes or devices.', &
'', &
'  2. It is also intended that it will cause data placed in an external', &
'     file by means other than the current Fortran process to be available', &
'     to the process in a subsequent READ statement.', &
'', &
'  Together, this is commonly called "flushing I/O buffers".', &
'', &
'  Note that execution of a FLUSH(3f) statement performs a wait operation', &
'  for all pending asynchronous data transfer operations for the specified', &
'  unit.', &
'', &
'  Execution of a FLUSH(3f) statement for a file that is connected but does', &
'  not exist is permitted and has no effect on any file.', &
'', &
'  A FLUSH(3f) statement has no effect on file position.', &
'', &
'  No specifier shall appear more than once in a given FLUSH(3f) statement.', &
'', &
'OPTIONS', &
'   [UNIT=]file-unit-number  Required. If the optional characters', &
'                            UNIT= are omitted from the unit specifier,', &
'                            the file-unit-number must be the first item.', &
'', &
'   ERR=label                The label must branch to a target statement', &
'                            in the same scoping unit as the FLUSH(3f)', &
'                            statement.', &
'', &
'RETURNS', &
'   IOSTAT=scalar-int-variable  variable is set to a processor-dependent', &
'                               positive value if an error occurs, to zero', &
'                               if the flush operation was successful, or', &
'                               to a processor-dependent negative value', &
'                               if the flush operation is not supported', &
'                               for the unit specified.', &
'', &
'   IOMSG=iomsg-variable    message describing any error that occurred', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_flush', &
'   implicit none', &
'   character(len=256) :: msg', &
'   integer :: ios, lun', &
'      lun=10', &
'      flush (unit=lun, iostat=ios, iomsg=msg)', &
'      if(ios.ne.0)then', &
'         write(*,''(a)'')''<ERROR>*flush*:''//trim(msg)', &
'      endif', &
'   end program demo_flush', &
'']

shortname="flush"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('94','fraction')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   fraction(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Fractional part of the', &
'   model representation', &
'', &
'DESCRIPTION', &
'   fraction(x) returns the fractional part of the model', &
'   representation of X.', &
'', &
'SYNTAX', &
'   y = fraction(x)', &
'', &
'ARGUMENTS', &
'   X   The type of the argument shall be a REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as the argument.', &
'   The fractional part of the model representation of X is returned;', &
'   it is x * radix(x)**(-exponent(x)).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_fraction', &
'    implicit none', &
'      real :: x', &
'      x = 178.1387e-4', &
'      print *, fraction(x), x * radix(x)**(-exponent(x))', &
'    end program demo_fraction', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'']

shortname="fraction"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('95','gamma')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   gamma(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Gamma function', &
'', &
'DESCRIPTION', &
'   gamma(x) computes Gamma of X. For positive,', &
'   integer values of X the Gamma function simplifies to the factorial', &
'   function Gamma(x)=(x-1)!.', &
'', &
'   $$', &
'   \Gamma(x) = \int_0**\infty t**{x-1}{\mathrm{e}}**{-t}\,{\mathrm{d}}t', &
'   $$', &
'', &
'SYNTAX', &
'   x = gamma(x)', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL and neither zero', &
'        nor a negative integer.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL of the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_gamma', &
'   implicit none', &
'     real :: x = 1.0', &
'     x = gamma(x) ! returns 1.0', &
'   end program demo_gamma', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   Logarithm of the Gamma function: [[log_gamma(3)', &
'', &
'']

shortname="gamma"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('96','get_command')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_command(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] Get the entire', &
'   command line', &
'', &
'SYNTAX', &
'   call get_command([command, length, status])', &
'', &
'    character(len=*),intent(out),optional :: command', &
'    integer,intent(out),optional :: length', &
'    integer,intent(out),optional :: status', &
'', &
'DESCRIPTION', &
'   Retrieve the entire command line that was used to invoke the program.', &
'', &
'   Note that what is typed on the command line is often processed by', &
'   a shell. The shell often processes special characters and white', &
'   space before passing it to the program. The processing can typically', &
'   be turned off by turning off globbing or quoting the command line', &
'   arguments with quote characters and/or changing the default field', &
'   separators, but this should rarely be necessary.', &
'', &
'RETURNS', &
'  COMMAND   (Optional) shall be of type CHARACTER and of default kind.', &
'            If COMMAND is present, stores the entire command line that', &
'            was used to invoke the program in COMMAND.', &
'  LENGTH    (Optional) Shall be of type INTEGER and of default kind.', &
'            If LENGTH is present, it is assigned the length of the', &
'            command line.', &
'  STATUS    (Optional) Shall be of type INTEGER and of default kind.', &
'            If STATUS is present, it is assigned 0 upon success of the', &
'            command, -1 if COMMAND is too short to store the command line,', &
'            or a positive value in case of an error.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_get_command', &
'   implicit none', &
'   integer                      :: COMMAND_LINE_LENGTH', &
'   character(len=:),allocatable :: COMMAND_LINE', &
'      ! get command line length', &
'      call get_command(length=COMMAND_LINE_LENGTH)', &
'      ! allocate string big enough to hold command line', &
'      allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)', &
'      ! get command line as a string', &
'      call get_command(command=COMMAND_LINE)', &
'      ! trim leading spaces just in case', &
'      COMMAND_LINE=adjustl(COMMAND_LINE)', &
'      write(*,''("OUTPUT:",a)'')COMMAND_LINE', &
'   end program demo_get_command', &
'', &
'  Sample execution:', &
'', &
'     # note that shell expansion removes some of the whitespace', &
'     # without quotes', &
'     ./test_get_command  arguments    on the    command   line to   echo', &
'', &
'     OUTPUT:./test_get_command arguments on the command line to echo', &
'', &
'     # using the bash shell with single quotes', &
'     ./test_get_command  ''arguments  *><`~[]!{}?"\''| on the    command   line ''', &
'', &
'     OUTPUT:./test_get_command arguments  *><`~[]!{}?"''| on the   command   line', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   get_command_argument(3), command_argument_count(3)', &
' JSU', &
'']

shortname="get_command"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('97','get_command_argument')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_command_argument(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] Get', &
'   command line arguments', &
'', &
'SYNTAX', &
'   call get_command_argument(number [, value, length, status])', &
'', &
'     integer,intent(in)                    :: number', &
'     character(len=*),intent(out),optional :: value', &
'     integer,intent(out),optional          :: length', &
'     integer,intent(out),optional          :: status', &
'', &
'DESCRIPTION', &
'   Retrieve the NUMBER-th argument that was passed on the command line', &
'   when the containing program was invoked.', &
'', &
'   There is not anything specifically stated about what an argument is but', &
'   in practice the arguments are split on whitespace unless the arguments', &
'   are quoted and IFS values (Internal Field Separators) used by common', &
'   shells are ignored.', &
'', &
'OPTIONS', &
'   NUMBER    Shall be a scalar of type integer(4), NUMBER >= 0.', &
'             If NUMBER = 0, VALUE is set to the name of the program', &
'             (on systems that support this feature).', &
'', &
'RETURNS', &
'   VALUE     Shall be a scalar of type CHARACTER and of default kind.', &
'             After get_command_argument returns, the VALUE argument', &
'             holds the NUMBER-th command line argument. If VALUE can', &
'             not hold the argument, it is truncated to fit the length', &
'             of VALUE. If there are less than NUMBER arguments specified', &
'             at the command line, VALUE will be filled with blanks.', &
'   LENGTH    (Optional) Shall be a scalar of type integer(4).', &
'             The LENGTH argument contains the length of the', &
'             NUMBER-th command line argument.', &
'   STATUS    (Optional) Shall be a scalar of type integer(4).', &
'             If the argument retrieval fails, STATUS is a positive number;', &
'             if VALUE contains a truncated command line argument, STATUS', &
'             is -1; and otherwise the STATUS is zero.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_get_command_argument', &
'   implicit none', &
'   character(len=255)           :: progname', &
'   integer                      :: stat', &
'   integer                      :: count,i, longest, argument_length', &
'   integer,allocatable          :: istat(:), ilen(:)', &
'   character(len=:),allocatable :: arguments(:)', &
'     ! get number of arguments', &
'     count = command_argument_count()', &
'     write(*,*)''The number of arguments is '',count', &
'     ! simple usage', &
'     call get_command_argument (0, progname, status=stat)', &
'     if (stat == 0) then', &
'        print *, "The program''s name is " // trim (progname)', &
'     endif', &
'     ! showing how to make an array to hold any argument list', &
'     ! find longest argument', &
'     longest=0', &
'     do i=0,count', &
'        call get_command_argument(number=i,length=argument_length)', &
'        longest=max(longest,argument_length)', &
'     enddo', &
'     ! allocate string array big enough to hold command line argument strings', &
'     ! and related information', &
'     allocate(character(len=longest) :: arguments(0:count))', &
'     allocate(istat(0:count))', &
'     allocate(ilen(0:count))', &
'     ! read the arguments into the array', &
'     do i=0,count', &
'       call get_command_argument(i, arguments(i),status=istat(i),length=ilen(i))', &
'     enddo', &
'     ! show the results', &
'     write (*,''(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")'') &', &
'     & (i,istat(i),ilen(i),arguments(i)(:ilen(i)),i=0,count)', &
'   end program demo_get_command_argument', &
'', &
'  Sample output:', &
'', &
'   ./test_get_command_argument a    test  ''of getting   arguments  '' "  leading"', &
'', &
'   > The number of arguments is            5', &
'   > The program''s name is xxx', &
'   >000 00000 00003 [./test_get_command_argument]', &
'   >001 00000 00001 [a]', &
'   >003 00000 00004 [test]', &
'   >004 00000 00024 [of getting   arguments  ]', &
'   >005 00000 00018 [  leading]', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   get_command(3), command_argument_count(3)', &
' JSU', &
'']

shortname="get_command_argument"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('98','get_environment_variable')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   get_environment_variable(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] Get', &
'   an environmental variable', &
'', &
'SYNTAX', &
'   call get_environment_variable(NAME[, VALUE, LENGTH, STATUS, TRIM_NAME)', &
'   character(len=*),intent(in)           :: NAME', &
'   character(len=*),intent(out),optional :: VALUE', &
'   integer,intent(out),optional          :: LENGTH', &
'   integer,intent(out),optional          :: STATUS', &
'   logical,intent(out),optional          :: TRIM_NAME', &
'', &
'DESCRIPTION', &
'   Get the VALUE of the environmental variable NAME.', &
'', &
'   Note that get_environment_variable(3f) need not be thread-safe. It is', &
'   the responsibility of the user to ensure that the environment is not', &
'   being updated concurrently.', &
'', &
'OPTIONS', &
'   NAME        (Optional) Shall be a scalar of type CHARACTER and of', &
'               default kind.', &
'', &
'RETURN VALUE', &
'   VALUE       (Optional) Shall be a scalar of type CHARACTER and of', &
'               default kind.', &
'               The value of NAME is stored in VALUE.', &
'               If VALUE is not large enough to hold the data, it is truncated.', &
'               If NAME is not set, VALUE will be filled with blanks.', &
'   LENGTH      (Optional) Shall be a scalar of type INTEGER and of default kind.', &
'               Argument LENGTH contains the length needed for storing', &
'               the environment variable NAME or zero if it is not present.', &
'   STATUS      (Optional) Shall be a scalar of type INTEGER and of', &
'               default kind. STATUS is -1 if VALUE is present but too', &
'               short for the environment variable;', &
'               it is 1 if the environment variable does not exist', &
'               and 2 if the processor does not support environment variables;', &
'               in all other cases STATUS is zero.', &
'   TRIM_NAME   (Optional) Shall be a scalar of type LOGICAL and of default kind.', &
'               If TRIM_NAME is present with the value .FALSE.,', &
'               the trailing blanks in NAME are significant;', &
'               otherwise they are not part of the environment variable name.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_getenv', &
'   implicit none', &
'   character(len=:),allocatable :: var', &
'   character(len=:),allocatable :: homedir', &
'   integer :: howbig, stat', &
'     var=''HOME''', &
'     ! get length required to hold value', &
'     call get_environment_variable(var, length=howbig,status=stat)', &
'     select case (stat)', &
'     case (1)', &
'        print *, "HOME is not defined in the environment. Strange..."', &
'     case (2)', &
'        print *, "This processor doesn''t support environment variables. Boooh!"', &
'     case default', &
'        ! make string to hold value of sufficient size', &
'        allocate(character(len=howbig) :: homedir)', &
'        ! get value', &
'        call get_environment_variable(var, homedir)', &
'        ! print environment variable name value', &
'        write (*,''(a,"=""",a,"""")'')var,trim(homedir)', &
'     end select', &
'   end program demo_getenv', &
'', &
'Typical Results:', &
'', &
'   HOME="/home/urbanjs"', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Subroutine', &
' JSU', &
'']

shortname="get_environment_variable"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('99','huge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   huge(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Largest number of a kind', &
'', &
'SYNTAX', &
'   result = huge(x)', &
'', &
'DESCRIPTION', &
'   huge(x) returns the largest number that is not an infinity in', &
'   the model of the type of X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL or INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_huge_tiny', &
'    ! or, "why I have my own NINT function"', &
'    implicit none', &
'    character(len=*),parameter :: f=''(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)''', &
'    integer :: i,j,k,biggest', &
'    real :: v, w', &
'      ! basic', &
'      print *, huge(0), huge(0.0), huge(0.0d0)', &
'      print *, tiny(0.0), tiny(0.0d0)', &
'', &
'      ! advanced', &
'      biggest=huge(0)', &
'      ! be careful when using integers in computation', &
'      do i=1,14', &
'         j=6**i   ! Danger, Danger', &
'         w=6**i   ! Danger, Danger', &
'         v=6.0**i', &
'         k=v      ! Danger, Danger', &
'         if(v.gt.biggest)then', &
'           write(*,f) i, j, k, v, v.eq.w, ''wrong j and k and w''', &
'        else', &
'           write(*,f) i, j, k, v, v.eq.w', &
'        endif', &
'      enddo', &
'    end program demo_huge_tiny', &
'', &
'     2147483647  3.4028235E+38  1.797693134862316E+308', &
'     1.1754944E-38  2.225073858507201E-308', &
'    1           6           6             6. T', &
'    2          36          36            36. T', &
'    3         216         216           216. T', &
'    4        1296        1296          1296. T', &
'    5        7776        7776          7776. T', &
'    6       46656       46656         46656. T', &
'    7      279936      279936        279936. T', &
'    8     1679616     1679616       1679616. T', &
'    9    10077696    10077696      10077696. T', &
'   10    60466176    60466176      60466176. T', &
'   11   362797056   362797056     362797056. T', &
'   12 -2118184960 -2147483648    2176782336. F wrong for j and k and w', &
'   13   175792128 -2147483648   13060694016. F wrong for j and k and w', &
'   14  1054752768 -2147483648   78364164096. F wrong for j and k and w', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="huge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('100','hypot')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   hypot(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Euclidean distance function', &
'', &
'SYNTAX', &
'   result = hypot(x, y)', &
'', &
'DESCRIPTION', &
'   hypot(x,y) is the Euclidean distance function. It is equal to', &
'', &
'      sqrt{X**2 + Y**2}, without undue underflow or overflow.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'   Y    The type and kind type parameter shall be the same as X.', &
'', &
'RETURN VALUE', &
'   The return value has the same type and kind type parameter as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_hypot', &
'    implicit none', &
'      real(4) :: x = 1.e0_4, y = 0.5e0_4', &
'      x = hypot(x,y)', &
'    end program demo_hypot', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="hypot"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('101','iachar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   iachar(3f) - [FORTRAN:INTRINSIC:CHARACTER] Code in ASCII collating sequence', &
'', &
'SYNTAX', &
'   result = iachar(c [, kind])', &
'', &
'DESCRIPTION', &
'   iachar(c) returns the code for the ASCII character', &
'   in the first character position of C.', &
'', &
'ARGUMENTS', &
'   C       Shall be a scalar CHARACTER, with intent(in)', &
'   KIND    (Optional) An INTEGER initialization', &
'expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_iachar', &
'   implicit none', &
'   ! create function to convert uppercase letters to lowercase', &
'      write(*,''(a)'')lower(''abcdefg ABCDEFG'')', &
'   contains', &
'   !', &
'   elemental pure function lower(str) result (string)', &
'   ! Changes a string to lowercase', &
'   character(*), intent(In)     :: str', &
'   character(len(str))          :: string', &
'   integer                      :: i', &
'      string = str', &
'      ! step thru each letter in the string in specified range', &
'      do i = 1, len(str)', &
'         select case (str(i:i))', &
'         case (''A'':''Z'') ! change letter to miniscule', &
'            string(i:i) = char(iachar(str(i:i))+32)', &
'         case default', &
'         end select', &
'      end do', &
'   end function lower', &
'   !', &
'   end program demo_iachar', &
'', &
'NOTE', &
'   See [[ichar]] for a discussion of converting between numerical', &
'   values and formatted string representations.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   [[achar]], [[char]], [[ichar]]', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="iachar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('102','iall')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   iall(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bitwise and of array elements', &
'', &
'SYNTAX', &
'   * result = iall(array[, mask])', &
'   * result = iall(array, dim[, mask])', &
'', &
'DESCRIPTION', &
'   Reduces with bitwise AND the elements of ARRAY along dimension DIM', &
'   if the corresponding element in MASK is TRUE.', &
'', &
'ARGUMENTS', &
'  ARRAY   Shall be an array of type INTEGER', &
'  DIM    (Optional) shall be a scalar of type INTEGER with a value', &
'         in the range from 1 to "n", where "n" equals the rank of ARRAY.', &
'  MASK   (Optional) shall be of type LOGICAL and either be a', &
'         scalar or an array of the same shape as ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as ARRAY.', &
'', &
'   If DIM is absent, a scalar with the bitwise ALL of all elements in', &
'   ARRAY is returned. Otherwise, an array of rank "n-1", where "n" equals', &
'   the rank of ARRAY, and a shape similar to that of ARRAY with', &
'   dimension DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_iall', &
'   use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'   & int8, int16, int32, int64', &
'   implicit none', &
'   integer(kind=int8) :: a(2)', &
'', &
'     a(1) = int(b''00100100'')', &
'     a(2) = int(b''01101010'')', &
'', &
'     ! prints 00100000', &
'     print ''(b8.8)'', iall(a)', &
'   end program demo_iall', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   iany(3), iparity(3), iand(3)', &
'']

shortname="iall"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('103','iand')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   iand(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bitwise logical and', &
'', &
'SYNTAX', &
'   result = iand(i, j)', &
'', &
'DESCRIPTION', &
'   Bitwise logical AND.', &
'', &
'ARGUMENTS', &
'   I    The type shall be INTEGER.', &
'   J    The type shall be INTEGER, of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The return type is INTEGER, of the same kind as the', &
'   arguments. (If the argument kinds differ, it is of the same kind as', &
'   the larger argument.)', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_iand', &
'    implicit none', &
'      integer :: a, b', &
'      data a / z''f'' /, b / z''3'' /', &
'      write (*,*) iand(a, b)', &
'    end program demo_iand', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ior(3), ieor(3), ibits(3), ibset(3), ibclr(3),', &
'   not(3)', &
'', &
'']

shortname="iand"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('104','iany')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   iany(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bitwise or of array elements', &
'', &
'SYNTAX', &
'   * result = iany(array[, mask])', &
'   * result = iany(array, dim[, mask])', &
'', &
'DESCRIPTION', &
'   Reduces with bitwise or (inclusive or) the elements of ARRAY along', &
'   dimension DIM if the corresponding element in MASK is TRUE.', &
'', &
'ARGUMENTS', &
'  ARRAY  Shall be an array of type INTEGER', &
'  DIM    (Optional) shall be a scalar of type INTEGER with a value', &
'         in the range from "1" to "n", where "n" equals the rank of ARRAY.', &
'  MASK   (Optional) shall be of type LOGICAL and either be a', &
'         scalar or an array of the same shape as ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as ARRAY.', &
'', &
'   If DIM is absent, a scalar with the bitwise OR of all elements in', &
'   ARRAY is returned. Otherwise, an array of rank "n-1", where "n"', &
'   equals the rank of ARRAY, and a shape similar to that of ARRAY', &
'   with dimension DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_iany', &
'   use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'   & int8, int16, int32, int64', &
'   implicit none', &
'   integer(kind=int8) :: a(2)', &
'     a(1) = int(b''00100100'')', &
'     a(2) = int(b''01101010'')', &
'     print ''(b8.8)'', iany(a)', &
'   end program demo_iany', &
'', &
'  Results:', &
'', &
'   01101110', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   iparity(3), iall(3), ior(3)', &
'']

shortname="iany"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('105','ibclr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ibclr(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Clear bit', &
'', &
'SYNTAX', &
'   result = ibclr(i, pos)', &
'', &
'DESCRIPTION', &
'   IBCLR returns the value of I with the bit at position', &
'   POS set to zero.', &
'', &
'ARGUMENTS', &
'   I     The type shall be INTEGER.', &
'   POS   The type shall be INTEGER.', &
'         A value of zero refers to the least significant bit.', &
'         POS is an INTENT(IN) scalar or array of type INTEGER.', &
'         The value of POS must be within the range zero to (BIT_SIZE(i)-1).', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ibits(3), ibset(3), iand(3), ior(3), ieor(3),', &
'   mvbits(3)', &
'', &
'']

shortname="ibclr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('106','ibits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ibits(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bit extraction', &
'', &
'SYNTAX', &
'   result = ibits(i, pos, len)', &
'', &
'DESCRIPTION', &
'   IBITS extracts a field of length LEN from I,', &
'   starting from bit position POS and extending left for LEN', &
'   bits. The result is right-justified and the remaining bits are', &
'   zeroed. The value of pos+len must be less than or equal to the', &
'   value bit_size(i).', &
'', &
'ARGUMENTS', &
'   I    The type shall be INTEGER.', &
'   POS  The type shall be INTEGER.', &
'        A value of zero refers to the least significant bit.', &
'   LEN  The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   bit_size(3), ibclr(3), ibset(3),', &
'   iand(3), ior(3), ieor(3)', &
'']

shortname="ibits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('107','ibset')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ibset(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Set bit', &
'', &
'SYNTAX', &
'   result = ibset(i, pos)', &
'', &
'DESCRIPTION', &
'   IBSET returns the value of I with the bit at position', &
'   POS set to one.', &
'', &
'ARGUMENTS', &
'   I    The type shall be INTEGER.', &
'   POS  The type shall be INTEGER.', &
'        A value of zero refers to the least significant bit.', &
'        pos is an INTENT(IN) scalar or array of type INTEGER.', &
'        The value of pos must be within the range zero to (BIT_SIZE(i)-1).', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   btest(3), ibclr(3), ibits(3), iand(3), ior(3), ieor(3),', &
'   mvbits(3)', &
'']

shortname="ibset"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('108','ichar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ichar(3f) - [FORTRAN:INTRINSIC:CHARACTER] Character-to-integer conversion', &
'   function', &
'', &
'SYNTAX', &
'   elemental function ichar(c,kind)', &
'', &
'    character(len=1),intent(in) :: c', &
'    integer,intent(in),optional :: kind', &
'', &
'DESCRIPTION', &
'   ICHAR(C) returns the code for the character in the system''s native', &
'   character set. The correspondence between characters and their codes', &
'   is not necessarily the same across different Fortran implementations.', &
'   For example, a platform using EBCDIC would return different values', &
'   than an ASCII platform.', &
'', &
'   See IACHAR(3f) for specifically working with the ASCII character set.', &
'', &
'ARGUMENTS', &
'   C       Shall be a scalar CHARACTER, with intent(in)', &
'   KIND   (Optional) An INTEGER initialization', &
'          expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_ichar', &
'   implicit none', &
'   integer i', &
'      write(*,*)ichar([''a'',''z'',''A'',''Z''])', &
'      do i=0,127', &
'         call printme()', &
'      enddo', &
'   contains', &
'   subroutine printme()', &
'   character(len=1) :: letter', &
'   letter=char(i)', &
'      select case(i)', &
'       case (:31,127:)', &
'         write(*,''(1x,i0.3,1x,"HEX=",z2.2,1x,i0)'')i,letter,ichar(letter)', &
'       case default', &
'         write(*,''(1x,i0.3,1x,a,1x,i0)'')i,letter,ichar(letter)', &
'      end select', &
'   end subroutine printme', &
'   end program demo_ichar', &
'', &
'NOTE', &
'   No intrinsic exists to convert between a numeric value and a formatted', &
'   character string representation -- for instance, given the', &
'   CHARACTER value ''154'', obtaining an INTEGER or', &
'   REAL value with the value 154, or vice versa. Instead, this', &
'   functionality is provided by internal-file I/O, as in the following', &
'   example:', &
'', &
'    program read_val', &
'      integer value', &
'      character(len=10) string, string2', &
'      string = ''154''', &
'', &
'      ! Convert a string to a numeric value', &
'      read (string,''(I10)'') value', &
'      print *, value', &
'', &
'      ! Convert a value to a formatted string', &
'      write (string2,''(I10)'') value', &
'      print *, string2', &
'    end program read_val', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   [[achar]], [[char]], [[iachar]]', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="ichar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('109','ieor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ieor(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bitwise logical exclusive or', &
'', &
'SYNTAX', &
'   result = ieor(i, j)', &
'', &
'DESCRIPTION', &
'   IEOR returns the bitwise Boolean exclusive-OR of I and J.', &
'', &
'ARGUMENTS', &
'   I   The type shall be INTEGER.', &
'   J   The type shall be INTEGER, of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The return type is INTEGER, of the same kind as the arguments. (If the', &
'   argument kinds differ, it is of the same kind as the larger argument.)', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ior(3), iand(3), ibits(3), ibset(3),', &
'   ibclr(3), not(3)', &
'', &
'']

shortname="ieor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('110','image_index')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   image_index(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Cosubscript to image', &
'   index conversion', &
'', &
'SYNTAX', &
'   result = image_index(coarray, sub)', &
'', &
'DESCRIPTION', &
'   Returns the image index belonging to a cosubscript.', &
'', &
'ARGUMENTS', &
'   COARRAY   Coarray of any type.', &
'   SUB       default integer rank-1 array of a size equal to the corank', &
'             of COARRAY.', &
'', &
'RETURN VALUE', &
'   Scalar default integer with the value of the image index which', &
'   corresponds to the cosubscripts. For invalid cosubscripts the result', &
'   is zero.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo image_index', &
'   implicit none', &
'   integer :: array[2,-1:4,8,*]', &
'      ! Writes  28 (or 0 if there are fewer than 28 images)', &
'      write (*,*) image_index(array, [2,0,3,1])', &
'   end demo image_index', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Inquiry function.', &
'', &
'SEE ALSO', &
'   this_image(3), num_images(3)', &
'']

shortname="image_index"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('111','include')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   include(7f) - [FORTRAN] including source text', &
'', &
'SYNOPSIS', &
'   INCLUDE char-literal-constant', &
'DESCRIPTION', &
'  Additional text may be incorporated into the source text of a program', &
'  unit during processing. This is accomplished with the INCLUDE line,', &
'  which typically has the form', &
'', &
'     INCLUDE "filename"', &
'', &
'  An INCLUDE line is not a Fortran statement. It is processed at', &
'  compilation. The effect of the INCLUDE line is as if the referenced', &
'  source text physically replaced the INCLUDE line prior to program', &
'  processing. Included text may contain any source text, including', &
'  additional INCLUDE lines; such nested INCLUDE lines are similarly', &
'  replaced with the specified source text. The maximum depth of nesting', &
'  of any nested INCLUDE lines is processor dependent. Inclusion of the', &
'  source text referenced by an INCLUDE line shall not, at any level of', &
'  nesting, result in inclusion of the same source text (ie. it cannot', &
'  be recursive).', &
'', &
'  The interpretation of char-literal-constant is processor dependent.', &
'  It is generally implemented  as a filename containing text to be', &
'  included, but could be interpreted as a URL or a system command that', &
'  generates text or a database query, or a list of files, for example.', &
'  That being said, all current implementations appear to at', &
'  least treat it as a simple filename.', &
'', &
'  Where the compiler searches for the filename is', &
'  implementation-dependent. All current implementations appear to at', &
'  least search for the file in the same directory as the file containing', &
'  the INCLUDE statement if it is not a complete filepath specification. It', &
'  is common but not required that other directories are searched as', &
'  specified with the common -I switch found on most compiler commands.', &
'', &
'  The char-literal-constant shall not have a kind type parameter value', &
'  that is a named-constant. That is, it must be a quoted string. It cannot', &
'  be something like', &
'', &
'      character(len=*),parameter :: filename=''willnotwork.inc''', &
'      include filename', &
'', &
'  An INCLUDE line shall appear on a single source line where a statement', &
'  may appear (many compilers support an extension allowing continuation', &
'  lines to be supported); it must be the only nonblank text on the line', &
'  other than an optional trailing comment (no statement label is allowed).', &
'  So here are some bad ideas', &
'', &
'      INCLUDE "filename";I=10 ! NO: multiple statements on line', &
'      100 INCLUDE ''filename''  ! NO: statement label not allowed', &
'      ! continuation often works but is non-standard', &
'      INCLUDE &', &
'      & ''filename''', &
'      INCLUDE ''file&', &
'      &name''', &
'', &
'  When an INCLUDE line is resolved, the first included statement line', &
'  cannot be a continuation line and the last included statement line', &
'  cannot be continued.', &
'', &
'  PREPROCESSING', &
'', &
'  Note that an INCLUDE line is generally processed after any preprocessor', &
'  so the INCLUDE file should not include preprocessor directives such as', &
'  cpp(1) or fpp(1) directives. If that is required you probably need to', &
'  use an equivalent preprocessor directive such as a cpp(1) "#include"', &
'  directive instead of a Fortran INCLUDE.', &
'', &
'  SUMMARY', &
'', &
'  So it is a de-facto standard that an INCLUDE at least supports a simple', &
'  filename pointing to a file in the directory where the file containing', &
'  the INCLUDE file resides or a full path name in single or double quotes.', &
'', &
'  An INCLUDE statement was a common way to ensure a COMMONBLOCK was', &
'  declared the same in multiple files (at least if every file with the', &
'  INCLUDE was recompiled). It should generally be avoided and a MODULE', &
'  should be used instead of a COMMONBLOCK in the vast majority of cases', &
'  in new code.', &
'', &
'  RULES FOR FIXED AND FREE FILE FORMAT PORTABILITY', &
'', &
'  If the code in your "include file" needs read by both old fixed-format', &
'  files and free-format files it is not necessary to maintain two copies', &
'  of the file.', &
'', &
'  Observing the following rules allows included code to be used with', &
'  either free or fixed source forms.', &
'', &
'   *   Confine statement labels to character positions 1 to', &
'       5 and statements to character positions 7 to 72', &
'   *   Treat blanks as being significant.', &
'   *   Use only the exclamation mark (!) to indicate', &
'       a comment, but do not start the comment in character', &
'       position 6.', &
'   *   For continued statements, place an ampersand (&) in', &
'       both character position 73 of a continued line and character', &
'       position 6 of a continuation line.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'     program show_include', &
'     implicit none', &
'     include "declarations.inc"', &
'        write(*,*)''Hello World!''', &
'        include "somecode.inc"', &
'     includes', &
'        include "somemorecode.inc"', &
'     end program show_include', &
'']

shortname="include"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('112','index')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   index(3f) - [FORTRAN:INTRINSIC:CHARACTER] Position of a substring within', &
'   a string', &
'', &
'SYNTAX', &
'    index(STRING, SUBSTRING [, BACK [, KIND]]) result(START)', &
'', &
'     character(len=*),intent(in) :: STRING', &
'     character(len=*),intent(in) :: SUBSTRING', &
'     logical,intent(in),optional :: BACK', &
'     integer,intent(in),optional :: KIND', &
'     integer(kind=KIND)          :: START', &
'', &
'DESCRIPTION', &
'   Returns the position of the start of the leftmost or rightmost', &
'   occurrence of string SUBSTRING in STRING, counting from one. If', &
'   SUBSTRING is not present in STRING, zero is returned.', &
'', &
'ARGUMENTS', &
'  STRING       string to be searched', &
'  SUBSTRING    string to attempt to locate in STRING', &
'  BACK         If the BACK argument is present and true, the return', &
'               value is the start of the rightmost occurrence rather than the', &
'               leftmost.', &
'  KIND         An INTEGER initialization expression indicating', &
'               the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'  START        The return value is of type INTEGER and of kind KIND.', &
'               If KIND is absent, the return value is of default integer', &
'               kind.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'EXAMPLE', &
'  Example program', &
'', &
'   program demo_index', &
'   implicit none', &
'                                     !1234567890123456789012345678901234567890', &
'   character(len=*),parameter :: str=''Search this string for this expression''', &
'      write(*,*)index(str,''this'').eq.8,              &', &
'                index(str,''this'',back=.true.).eq.24, &', &
'                ! INDEX is case-sensitive', &
'                index(str,''This'').eq.0', &
'   end program demo_index', &
'', &
'  Expected Results:', &
'', &
'   > T T T', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="index"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('113','int')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   int(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Convert to integer type', &
'', &
'SYNTAX', &
'   result = int(a [, kind))', &
'', &
'DESCRIPTION', &
'   Convert to integer type', &
'', &
'ARGUMENTS', &
'   A      Shall be of type INTEGER,', &
'          REAL, or COMPLEX.', &
'   KIND   (Optional) An INTEGER initialization', &
'          expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   These functions return a INTEGER variable or array under', &
'   the following rules:', &
'', &
'    1. If A is of type INTEGER, int(a) = a', &
'', &
'    2. If A is of type REAL and |a| < 1, int(a) equals 0.', &
'       If |a| >= 1, then int(a) equals the largest integer that', &
'       does not exceed the range of A and whose sign is the same as the', &
'       sign of A.', &
'', &
'    3. If A is of type COMPLEX, rule 2 is applied to the real part of A.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_int', &
'    implicit none', &
'      integer :: i = 42', &
'      complex :: z = (-3.7, 1.0)', &
'      print *, int(i)', &
'      print *, int(z), int(z,8)', &
'    end program demo_int', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="int"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('114','ior')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ior(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bitwise logical inclusive or', &
'', &
'SYNTAX', &
'   result = ior(i, j)', &
'', &
'    integer,intent(in) :: i', &
'    integer,intent(in) :: j', &
'', &
'DESCRIPTION', &
'   IOR returns the bit-wise Boolean inclusive-OR of I and J.', &
'', &
'ARGUMENTS', &
'   I   an INTEGER scalar or array.', &
'   J   INTEGER scalar or array, of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The return type is INTEGER, of the same kind as the arguments. (If the', &
'   argument kinds differ, it is of the same kind as the larger argument.)', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_ior', &
'   implicit none', &
'   integer :: i, j, k', &
'      i=53       ! i=00110101 binary (lowest order byte)', &
'      j=45       ! j=00101101 binary (lowest order byte)', &
'      k=ior(i,j) ! k=00111101 binary (lowest order byte) , k=61 decimal', &
'      write(*,''(i8,1x,b8.8)'')i,i,j,j,k,k', &
'   end program demo_ior', &
'', &
'  Results:', &
'', &
'         53 00110101', &
'         45 00101101', &
'         61 00111101', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ieor(3), iand(3), ibits(3), ibset(3),', &
'   ibclr(3), not(3)', &
'']

shortname="ior"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('115','iparity')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   iparity(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Bitwise exclusive or of', &
'   array elements', &
'', &
'SYNTAX', &
'   * result = iparity(array[, mask])', &
'   * result = iparity(array, dim[, mask])', &
'', &
'DESCRIPTION', &
'   Reduces with bitwise XOR (exclusive OR) the elements of ARRAY', &
'   along dimension DIM if the corresponding element in MASK is', &
'   TRUE.', &
'', &
'ARGUMENTS', &
'   ARRAY   Shall be an array of type INTEGER', &
'   DIM     (Optional) shall be a scalar of type INTEGER with a value', &
'           in the range from "1" to "n", where "n" equals the rank of ARRAY.', &
'   MASK    (Optional) shall be of type LOGICAL and either be a', &
'           scalar or an array of the same shape as ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as ARRAY.', &
'', &
'   If DIM is absent, a scalar with the bitwise XOR of all elements', &
'   in ARRAY is returned. Otherwise, an array of rank "n-1", where "n"', &
'   equals the rank of ARRAY, and a shape similar to that of ARRAY with', &
'   dimension DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_iparity', &
'   implicit none', &
'     integer, dimension(2) :: a', &
'     a(1) = int(b''00100100'')', &
'     a(2) = int(b''01101010'')', &
'     print ''(b8.8)'', iparity(a)', &
'   end program demo_iparity', &
'', &
'  Results:', &
'', &
'   01001110', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   iany(3), iall(3), ieor(3), parity(3)', &
'']

shortname="iparity"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('116','is_contiguous')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   is_contiguous(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] test if object is', &
'   contiguous', &
'', &
'SYNTAX', &
'    result = is_contiguous(A)', &
'', &
'DESCRIPTION', &
'   True if and only if an object is contiguous.', &
'', &
'   An object is contiguous if it is', &
'      (1)     an object with the CONTIGUOUS attribute,', &
'      (2)     a nonpointer whole array that is not assumed-shape,', &
'      (3)     an assumed-shape array that is argument associated with', &
'              an array that is contiguous,', &
'      (4)     an array allocated by an ALLOCATE statement,', &
'      (5)     a pointer associated with a contiguous target, or', &
'      (6)     a nonzero-sized array section provided that', &
'          (a)   its base object is contiguous,', &
'          (b)   it does not have a vector subscript,', &
'          (c)   the elements of the section, in array element order,', &
'                are a subset of the base object elements', &
'                that are consecutive in array element order,', &
'          (d)   if the array is of type character and a substring-range', &
'                appears, the substring-range specifies all of the', &
'                characters of the parent-string,', &
'          (e)   only its final part-ref has nonzero rank, and', &
'          (f)   it is not the real or imaginary part of an array of', &
'                type complex.', &
'', &
'   An object is not contiguous if it is an array subobject, and', &
'', &
'      o the object has two or more elements,', &
'      o the elements of the object in array element order are not', &
'        consecutive in the elements of the base object,', &
'      o the object is not of type character with length zero, and', &
'      o the object is not of a derived type that has no ultimate', &
'        components other than zero-sized arrays and', &
'      o characters with length zero.', &
'', &
'   It is processor-dependent whether any other object is contiguous.', &
'', &
'ARGUMENTS', &
'    A         may be of any type. It shall be an array. If it is a', &
'              pointer it shall be associated.', &
'', &
'RETURN VALUE', &
'    Result    of type Default logical scalar.', &
'              The result has the value true if A is contiguous, and', &
'              false otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_is_contiguous', &
'   implicit none', &
'   intrinsic is_contiguous', &
'   REAL, DIMENSION (1000, 1000), TARGET :: A', &
'   REAL, DIMENSION (:, :), POINTER       :: IN, OUT', &
'   IN => A              ! Associate IN with target A', &
'   OUT => A(1:1000:2,:) ! Associate OUT with subset of target A', &
'   !', &
'   write(*,*)''IN is '',IS_CONTIGUOUS(IN)', &
'   write(*,*)''OUT is '',IS_CONTIGUOUS(OUT)', &
'   !', &
'   end program demo_is_contiguous', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="is_contiguous"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('117','ishft')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ishft(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Shift bits', &
'', &
'SYNTAX', &
'   result = ishft(i, shift)', &
'', &
'DESCRIPTION', &
'   ISHFT returns a value corresponding to I with all of the', &
'   bits shifted SHIFT places. A value of SHIFT greater than', &
'   zero corresponds to a left shift, a value of zero corresponds to no', &
'   shift, and a value less than zero corresponds to a right shift. If the', &
'   absolute value of SHIFT is greater than bit_size(i), the', &
'   value is undefined. Bits shifted out from the left end or right end are', &
'   lost; zeros are shifted in from the opposite end.', &
'', &
'ARGUMENTS', &
'   I       The type shall be INTEGER.', &
'   SHIFT   The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ishftc(3)', &
'']

shortname="ishft"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('118','ishftc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ishftc(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Shift bits circularly', &
'', &
'SYNTAX', &
'   result = ishftc(i, shift [, size])', &
'', &
'DESCRIPTION', &
'   ISHFTC returns a value corresponding to I with the', &
'   rightmost SIZE bits shifted circularly SHIFT places; that', &
'   is, bits shifted out one end are shifted into the opposite end. A value', &
'   of SHIFT greater than zero corresponds to a left shift, a value of', &
'   zero corresponds to no shift, and a value less than zero corresponds to', &
'   a right shift. The absolute value of SHIFT must be less than', &
'   SIZE. If the SIZE argument is omitted, it is taken to be', &
'   equivalent to bit_size(i).', &
'', &
'ARGUMENTS', &
'   I        The type shall be INTEGER.', &
'   SHIFT    The type shall be INTEGER.', &
'   SIZE     (Optional) The type shall be INTEGER;', &
'            the value must be greater than zero and less than or equal to', &
'            bit_size(i).', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   ishft(3)', &
'', &
'']

shortname="ishftc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('119','is_iostat_end')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   is_iostat_end(3f) - [FORTRAN:INTRINSIC] Test for end-of-file value', &
'', &
'SYNTAX', &
'  function is_iostat_end(i)', &
'', &
'    logical            :: is_iostat_end (i)', &
'    integer,intent(in) :: i', &
'', &
'DESCRIPTION', &
'   is_iostat_end tests whether an variable has the value of the I/O', &
'   status "end of file". The function is equivalent to comparing the', &
'   variable with the iostat_end parameter of the intrinsic module', &
'   [[iso_fortran_env]].', &
'', &
'ARGUMENTS', &
'   I   Shall be of the type INTEGER.', &
'', &
'RETURN VALUE', &
'   Returns a LOGICAL of the default kind, which .true. if', &
'   I has the value which indicates an end of file condition for', &
'   IOSTAT= specifiers, and is .false. otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_iostat', &
'      implicit none', &
'      integer :: stat, i', &
'      open(88, file=''test.dat'')', &
'      read(88, *, iostat=stat) i', &
'      if(is_iostat_end(stat)) stop ''end of file''', &
'    end program demo_iostat', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'']

shortname="is_iostat_end"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('120','is_iostat_eor')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   is_iostat_eor(3f) - [FORTRAN:INTRINSIC] Test for end-of-record value', &
'', &
'SYNTAX', &
'   result = is_iostat_eor(i)', &
'', &
'DESCRIPTION', &
'   is_iostat_eor tests whether an variable has the value of the I/O', &
'   status "end of record". The function is equivalent to comparing the', &
'   variable with the iostat_eor parameter of the intrinsic module', &
'   [[iso_fortran_env]].', &
'', &
'ARGUMENTS', &
'   I   Shall be of the type INTEGER.', &
'', &
'RETURN VALUE', &
'   Returns a LOGICAL of the default kind, which .true. if', &
'   I has the value which indicates an end of file condition for', &
'   iostat= specifiers, and is .false. otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_is_iostat_eor', &
'      implicit none', &
'      integer :: stat, i(50)', &
'      open(88, file=''test.dat'', form=''unformatted'')', &
'      read(88, iostat=stat) i', &
'      if(is_iostat_eor(stat)) stop ''end of record''', &
'    end program demo_is_iostat_eor', &
'', &
'STANDARD', &
'   Fortran 2003 and later', &
'', &
'CLASS', &
'   Elemental function', &
'']

shortname="is_iostat_eor"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('121','kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   kind(3f) - [FORTRAN:INTRINSIC:KIND INQUIRY] Kind of an entity', &
'', &
'SYNTAX', &
'   k = kind(x)', &
'', &
'DESCRIPTION', &
'   kind(x) returns the kind value of the entity X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type LOGICAL, INTEGER,', &
'        REAL, COMPLEX or CHARACTER.', &
'', &
'RETURN VALUE', &
'   The return value is a scalar of type INTEGER and of the default', &
'   integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_kind', &
'    implicit none', &
'      integer,parameter :: kc = kind('' '')', &
'      integer,parameter :: kl = kind(.true.)', &
'', &
'    print *, "The default character kind is ", kc', &
'      print *, "The default logical kind is ", kl', &
'    end program demo_kind', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('122','lbound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   lbound(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] Lower dimension bounds of', &
'   an array', &
'', &
'SYNTAX', &
'   result = lbound(array [, dim [, kind]])', &
'', &
'DESCRIPTION', &
'   Returns the lower bounds of an array, or a single lower bound', &
'   along the DIM dimension.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array, of any type.', &
'   DIM      (Optional) Shall be a scalar INTEGER.', &
'   KIND     (Optional) An INTEGER initialization', &
'            expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If KIND is', &
'   absent, the return value is of default integer kind. If DIM is absent,', &
'   the result is an array of the lower bounds of ARRAY. If DIM is present,', &
'   the result is a scalar corresponding to the lower bound of the array', &
'   along that dimension. If ARRAY is an expression rather than a whole', &
'   array or array structure component, or if it has a zero extent along', &
'   the relevant dimension, the lower bound is taken to be 1.', &
'EXAMPLE', &
'   Note that in my opinion this function should not be used on', &
'   assumed-size arrays or in any function without an explicit', &
'   interface. Errors can occur if there is no interface defined.', &
'', &
'  Sample program', &
'', &
'   ! program demo_lbound', &
'   module m_bounds', &
'   implicit none', &
'    contains', &
'       subroutine msub(arr)', &
'          !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array', &
'          integer,intent(in) :: arr(:)', &
'          write(*,*)''MSUB: LOWER='',lbound(arr), &', &
'          & ''UPPER='',ubound(arr), &', &
'          & ''SIZE='',size(arr)', &
'       end subroutine msub', &
'    end module m_bounds', &
'', &
'    use m_bounds, only : msub', &
'    implicit none', &
'    interface', &
'       subroutine esub(arr)', &
'       integer,intent(in) :: arr(:)', &
'       end subroutine esub', &
'    end interface', &
'    integer :: arr(-10:10)', &
'       write(*,*)''MAIN: LOWER='',lbound(arr), &', &
'       & ''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'       call csub()', &
'       call msub(arr)', &
'       call esub(arr)', &
'    contains', &
'       subroutine csub', &
'          write(*,*)''CSUB: LOWER='',lbound(arr), &', &
'          & ''UPPER='',ubound(arr), &', &
'          & ''SIZE='',size(arr)', &
'       end subroutine csub', &
'    end', &
'', &
'    subroutine esub(arr)', &
'    implicit none', &
'    integer,intent(in) :: arr(:)', &
'       ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE', &
'       ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)', &
'       write(*,*)''ESUB: LOWER='',lbound(arr), &', &
'       & ''UPPER='',ubound(arr), &', &
'       & ''SIZE='',size(arr)', &
'    end subroutine esub', &
'   !end program demo_lbound', &
'', &
'  Expected output', &
'', &
'   MAIN: LOWER=         -10 UPPER=          10 SIZE=          21', &
'   CSUB: LOWER=         -10 UPPER=          10 SIZE=          21', &
'   MSUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'   ESUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   ubound(3), co_lbound(3)', &
'']

shortname="lbound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('123','leadz')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   leadz(3f) - [FORTRAN:INTRINSIC:BIT INQUIRY] Number of leading zero', &
'   bits of an integer', &
'', &
'SYNTAX', &
'   result = leadz(i)', &
'', &
'DESCRIPTION', &
'   LEADZ returns the number of leading zero bits of an integer.', &
'', &
'ARGUMENTS', &
'   I  Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The type of the return value is the same as a default INTEGER.', &
'   If all the bits of I are zero, the result value is bit_size(i).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_leadz', &
'    implicit none', &
'    integer :: value, i', &
'    character(len=80) :: f', &
'      write(*,''(*(g0))'')''BIT_SIZE='',bit_size(value)', &
'      ! make a format statement for writing a value as a bit string', &
'      write(f,''("(b",i0,".",i0,")")'')bit_size(value),bit_size(value)', &
'      ! show output for various integer values', &
'      value=0', &
'      do i=0,bit_size(value)-1', &
'         write (*,''("LEADING ZERO BITS=",i3,1x)'') leadz(value)', &
'         write (*,''(" FOR VALUE ")'',advance=''no'')', &
'         write(*,f,advance=''no'') value', &
'         write(*,''(*(1x,g0))'') "OR",value', &
'         value=value+2**(i)', &
'      enddo', &
'    end program demo_leadz', &
'  Results:', &
'', &
'   BIT_SIZE=32', &
'   LEADING ZERO BITS= 32', &
'    FOR VALUE 00000000000000000000000000000000 OR 0', &
'   LEADING ZERO BITS= 31', &
'    FOR VALUE 00000000000000000000000000000001 OR 1', &
'   LEADING ZERO BITS= 30', &
'    FOR VALUE 00000000000000000000000000000011 OR 3', &
'   LEADING ZERO BITS= 29', &
'    FOR VALUE 00000000000000000000000000000111 OR 7', &
'   LEADING ZERO BITS= 28', &
'    FOR VALUE 00000000000000000000000000001111 OR 15', &
'   LEADING ZERO BITS= 27', &
'    FOR VALUE 00000000000000000000000000011111 OR 31', &
'   LEADING ZERO BITS= 26', &
'    FOR VALUE 00000000000000000000000000111111 OR 63', &
'   LEADING ZERO BITS= 25', &
'    FOR VALUE 00000000000000000000000001111111 OR 127', &
'   LEADING ZERO BITS= 24', &
'    FOR VALUE 00000000000000000000000011111111 OR 255', &
'   LEADING ZERO BITS= 23', &
'    FOR VALUE 00000000000000000000000111111111 OR 511', &
'   LEADING ZERO BITS= 22', &
'    FOR VALUE 00000000000000000000001111111111 OR 1023', &
'   LEADING ZERO BITS= 21', &
'    FOR VALUE 00000000000000000000011111111111 OR 2047', &
'   LEADING ZERO BITS= 20', &
'    FOR VALUE 00000000000000000000111111111111 OR 4095', &
'   LEADING ZERO BITS= 19', &
'    FOR VALUE 00000000000000000001111111111111 OR 8191', &
'   LEADING ZERO BITS= 18', &
'    FOR VALUE 00000000000000000011111111111111 OR 16383', &
'   LEADING ZERO BITS= 17', &
'    FOR VALUE 00000000000000000111111111111111 OR 32767', &
'   LEADING ZERO BITS= 16', &
'    FOR VALUE 00000000000000001111111111111111 OR 65535', &
'   LEADING ZERO BITS= 15', &
'    FOR VALUE 00000000000000011111111111111111 OR 131071', &
'   LEADING ZERO BITS= 14', &
'    FOR VALUE 00000000000000111111111111111111 OR 262143', &
'   LEADING ZERO BITS= 13', &
'    FOR VALUE 00000000000001111111111111111111 OR 524287', &
'   LEADING ZERO BITS= 12', &
'    FOR VALUE 00000000000011111111111111111111 OR 1048575', &
'   LEADING ZERO BITS= 11', &
'    FOR VALUE 00000000000111111111111111111111 OR 2097151', &
'   LEADING ZERO BITS= 10', &
'    FOR VALUE 00000000001111111111111111111111 OR 4194303', &
'   LEADING ZERO BITS=  9', &
'    FOR VALUE 00000000011111111111111111111111 OR 8388607', &
'   LEADING ZERO BITS=  8', &
'    FOR VALUE 00000000111111111111111111111111 OR 16777215', &
'   LEADING ZERO BITS=  7', &
'    FOR VALUE 00000001111111111111111111111111 OR 33554431', &
'   LEADING ZERO BITS=  6', &
'    FOR VALUE 00000011111111111111111111111111 OR 67108863', &
'   LEADING ZERO BITS=  5', &
'    FOR VALUE 00000111111111111111111111111111 OR 134217727', &
'   LEADING ZERO BITS=  4', &
'    FOR VALUE 00001111111111111111111111111111 OR 268435455', &
'   LEADING ZERO BITS=  3', &
'    FOR VALUE 00011111111111111111111111111111 OR 536870911', &
'   LEADING ZERO BITS=  2', &
'    FOR VALUE 00111111111111111111111111111111 OR 1073741823', &
'   LEADING ZERO BITS=  1', &
'    FOR VALUE 01111111111111111111111111111111 OR 2147483647', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   bit_size(3), popcnt(3), poppar(3), trailz(3)', &
'']

shortname="leadz"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('124','len')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   len(3f) - [FORTRAN:INTRINSIC:CHARACTER] Length of a character entity', &
'', &
'SYNTAX', &
'   l = len(string [, kind])', &
'', &
'DESCRIPTION', &
'   Returns the length of a character string. If STRING is an array,', &
'   the length of an element of STRING is returned. Note that', &
'   STRING need not be defined when this intrinsic is invoked, since', &
'   only the length, not the content, of STRING is needed.', &
'', &
'ARGUMENTS', &
'   STRING  Shall be a scalar or array of type', &
'           CHARACTER, with intent(in)', &
'   KIND    (Optional) An INTEGER initialization', &
'           expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'EXAMPLE', &
'  Sample program', &
'', &
'     program demo_len', &
'     implicit none', &
'     character(len=:),allocatable :: string', &
'        string='' how long is this string?     ''', &
'        write(*,*)''LENGTH='',len(string)', &
'        write(*,*)''TRIMMED LENGTH='',len_trim(string)', &
'     end program demo_len', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="len"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('125','len_trim')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   len_trim(3f) - [FORTRAN:INTRINSIC:CHARACTER] Length of a character', &
'   entity without trailing blank characters', &
'', &
'SYNTAX', &
'   result = len_trim(string [, kind])', &
'', &
'    character(len=*),intent(in) :: string', &
'    integer,intent(in) :: kind', &
'', &
'DESCRIPTION', &
'   Returns the length of a character string, ignoring any trailing blanks.', &
'', &
'ARGUMENTS', &
'   STRING    Shall be a scalar of type CHARACTER, with intent(in)', &
'   KIND      (Optional) An INTEGER initialization', &
'             expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program', &
'', &
'     program demo_len_trim', &
'     implicit none', &
'     character(len=:),allocatable :: string', &
'        string='' how long is this string?     ''', &
'        write(*,*)''LENGTH='',len(string)', &
'        write(*,*)''TRIMMED LENGTH='',len_trim(string)', &
'        !', &
'        ELE:block ! elemental example', &
'        character(len=:),allocatable :: tablet(:)', &
'        tablet=[character(len=256) :: &', &
'        & '' how long is this string?     '',&', &
'        & ''and this one?'']', &
'           write(*,*)''LENGTH='',len(tablet)', &
'           write(*,*)''TRIMMED LENGTH='',len_trim(tablet)', &
'           write(*,*)''SUM TRIMMED LENGTH='',sum(len_trim(tablet))', &
'        endblock ELE', &
'        !', &
'     end program demo_len_trim', &
'  Results:', &
'', &
'    LENGTH=          30', &
'    TRIMMED LENGTH=          25', &
'    LENGTH=         256', &
'    TRIMMED LENGTH=          25          13', &
'    SUM TRIMMED LENGTH=          38', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
' JSU', &
'']

shortname="len_trim"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('126','lge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   lge(3f) - [FORTRAN:INTRINSIC:CHARACTER] Lexical greater than or equal', &
'', &
'SYNTAX', &
'   result = lge(string_a, string_b)', &
'', &
'DESCRIPTION', &
'   Determines whether one string is lexically greater than or equal to', &
'   another string, where the two strings are interpreted as containing', &
'   ASCII character codes. If the String A and String B are not the same', &
'   length, the shorter is compared as if spaces were appended to it to', &
'   form a value that has the same length as the longer.', &
'', &
'   In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT', &
'   differ from the corresponding intrinsic operators .ge., .gt., .le.,', &
'   and .lt., in that the latter use the processor''s character ordering', &
'   (which is not ASCII on some targets), whereas the former always use', &
'   the ASCII ordering.', &
'', &
'ARGUMENTS', &
'   string_a    Shall be of default CHARACTER type.', &
'   string_b    Shall be of default CHARACTER type.', &
'', &
'RETURN VALUE', &
'   Returns .true. if string_a >= string_b, and .false.', &
'   otherwise, based on the ASCII ordering.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   [[lgt(3), [[lle(3), [[llt(3)', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="lge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('127','lgt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   lgt(3f) - [FORTRAN:INTRINSIC:CHARACTER] Lexical greater than', &
'', &
'SYNTAX', &
'   result = lgt(string_a, string_b)', &
'', &
'DESCRIPTION', &
'   Determines whether one string is lexically greater than another string,', &
'   where the two strings are interpreted as containing ASCII character', &
'   codes. If the String A and String B are not the same length, the', &
'   shorter is compared as if spaces were appended to it to form a value', &
'   that has the same length as the longer.', &
'', &
'   In general, the lexical comparison intrinsics LGE, LGT,', &
'   LLE, and LLT differ from the corresponding intrinsic', &
'   operators .ge., .gt., .le., and .lt., in', &
'   that the latter use the processor''s character ordering (which is not', &
'   ASCII on some targets), whereas the former always use the ASCII', &
'   ordering.', &
'', &
'ARGUMENTS', &
'   string_a    Shall be of default CHARACTER type.', &
'   string_b    Shall be of default CHARACTER type.', &
'', &
'RETURN VALUE', &
'   Returns .true. if string_a > string_b, and .false.', &
'   otherwise, based on the ASCII ordering.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   lge(3), lle(3), llt(3)', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="lgt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('128','lle')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   lle(3f) - [FORTRAN:INTRINSIC:CHARACTER] Lexical less than or equal', &
'', &
'SYNTAX', &
'   result = lle(STR_A, STR_B)', &
'', &
'   character(len=*),intent(in) :: STR_A, STR_B', &
'    or', &
'   character(len=*),intent(in) :: STR_A, STR_B(*)', &
'   logical :: result', &
'', &
'DESCRIPTION', &
'   Determines whether one string is lexically less than or equal to', &
'   another string, where the two strings are interpreted as containing', &
'   ASCII character codes. If the String A and String B are not the same', &
'   length, the shorter is compared as if spaces were appended to it to', &
'   form a value that has the same length as the longer. Leading spaces', &
'   are significant.', &
'', &
'   In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT', &
'   differ from the corresponding intrinsic operators .ge., .gt., .le.,', &
'   and .lt., in that the latter use the processor''s character ordering', &
'   (which is not ASCII on some targets), whereas the former always use', &
'   the ASCII ordering.', &
'', &
'ARGUMENTS', &
'   STR_A    variable or array of default CHARACTER type.', &
'   STR_B    variable or array of default CHARACTER type.', &
'', &
'            if STR_A and STR_B are both arrays they must be of the', &
'            same shape.', &
'', &
'RETURN VALUE', &
'   RESULT   Returns .TRUE. if STR_A <= STR_B, and .FALSE.', &
'            otherwise, based on the ASCII ordering.', &
'', &
'EXAMPLE', &
' Sample program:', &
'', &
'   program demo_lle', &
'   implicit none', &
'   integer             :: i', &
'      write(*,''(*(a))'')(char(i),i=32,126)', &
'        write(*,*) lle(''abc'',''ABC'')              ! F lowercase is > uppercase', &
'        write(*,*) lle(''abc'',''abc  '')            ! T trailing spaces', &
'        ! If both strings are of zero length the result is true.', &
'        write(*,*) lle('''','''')                    ! T', &
'        write(*,*) lle('''',''a'')                   ! T the null string is padded', &
'        write(*,*) lle(''a'','''')                   ! F', &
'        write(*,*) lle(''abc'',[''abc'',''123''])      ! [T,F] scalar and array', &
'        write(*,*) lle([''cba'', ''123''],''abc'')     ! [F,T]', &
'        write(*,*) lle([''abc'',''123''],[''cba'',''123'']) ! [T,T] both arrays', &
'   end program demo_lle', &
'', &
'  Results:', &
'', &
'   > !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
'   > [\]^_`abcdefghijklmnopqrstuvwxyz{|}~', &
'   > F', &
'   > T', &
'   > T', &
'   > T', &
'   > F', &
'   > T F', &
'   > F T', &
'   > T T', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   lge(3), lgt(3), llt(3)', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="lle"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('129','llt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   llt(3f) - [FORTRAN:INTRINSIC:CHARACTER] Lexical less than', &
'', &
'SYNTAX', &
'   result = llt(string_a, string_b)', &
'', &
'DESCRIPTION', &
'   Determines whether one string is lexically less than another string,', &
'   where the two strings are interpreted as containing ASCII character', &
'   codes. If the String A and String B are not the same length, the', &
'   shorter is compared as if spaces were appended to it to form a value', &
'   that has the same length as the longer.', &
'', &
'   In general, the lexical comparison intrinsics LGE, LGT,', &
'   LLE, and LLT differ from the corresponding intrinsic', &
'   operators .ge., .gt., .le., and .lt., in', &
'   that the latter use the processor''s character ordering (which is not', &
'   ASCII on some targets), whereas the former always use the ASCII', &
'   ordering.', &
'', &
'ARGUMENTS', &
'   string_a    Shall be of default CHARACTER type.', &
'   string_b    Shall be of default CHARACTER type.', &
'', &
'RETURN VALUE', &
'   Returns .true. if string_a <= string_b, and .false.', &
'   otherwise, based on the ASCII ordering.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   [[lge(3), [[lgt(3), [[lle(3)', &
'', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="llt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('130','log10')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   log10(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Base 10 logarithm function', &
'', &
'SYNTAX', &
'   result = LOG10(x)', &
'', &
'DESCRIPTION', &
'   LOG10(X) computes the base 10 logarithm of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL or COMPLEX.', &
'   The kind type parameter is the same as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_log10', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'    real(kind=real64) :: x = 10.0_real64', &
'      x = log10(x)', &
'    end program demo_log10', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="log10"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('131','log')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   log(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Logarithm function', &
'', &
'SYNTAX', &
'   result = LOG(X)', &
'', &
'DESCRIPTION', &
'   LOG(X) computes the natural logarithm of X, i.e. the logarithm to', &
'   the base "e".', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL or COMPLEX.', &
'   The kind type parameter is the same as X.', &
'   If X is COMPLEX, the imaginary part OMEGA is in the range', &
'', &
'   -PI < OMEGA <= PI.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_log', &
'   implicit none', &
'     real(kind(0.0d0)) :: x = 2.71828182845904518d0', &
'     complex :: z = (1.0, 2.0)', &
'     x = log(x)    ! will yield (approximately) 1', &
'     z = log(z)', &
'   end program demo_log', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'']

shortname="log"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('132','log_gamma')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   log_gamma(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Logarithm of the', &
'   Gamma function', &
'', &
'SYNTAX', &
'    x = log_gamma(x)', &
'', &
'DESCRIPTION', &
'   log_gamma(x) computes the natural logarithm of the absolute value of', &
'   the [[Gamma function]].', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL and neither zero nor a negative integer.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL of the same kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_log_gamma', &
'   implicit none', &
'     real :: x = 1.0', &
'     x = log_gamma(x) ! returns 0.0', &
'   end program demo_log_gamma', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   Gamma function: gamma(3)', &
'']

shortname="log_gamma"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('133','logical')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   logical(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Converts one', &
'    kind of LOGICAL variable to another', &
'', &
'SYNTAX', &
'   result = logical(l [, kind])', &
'', &
'DESCRIPTION', &
'   Converts one kind of LOGICAL variable to another.', &
'', &
'ARGUMENTS', &
'   L      The type shall be LOGICAL.', &
'   KIND   (Optional) An INTEGER initialization', &
'expression indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is a LOGICAL value equal to L, with a', &
'   kind corresponding to KIND, or of the default logical kind if', &
'   KIND is not given.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   int(3), real(3), cmplx(3)', &
'']

shortname="logical"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('134','maskl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   maskl(3f) - [FORTRAN:INTRINSIC] Left justified mask', &
'', &
'SYNOPSIS', &
'   result = maskl(i[, kind])', &
'', &
'DESCRIPTION', &
'   maskl(i[, kind]) has its leftmost I bits set to 1, and the', &
'   remaining bits set to 0.', &
'', &
'SYNTAX', &
'', &
'ARGUMENTS', &
'   I     Shall be of type INTEGER.', &
'   KIND  Shall be a scalar constant expression of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER. If KIND is present, it', &
'   specifies the kind value of the return type; otherwise, it is of the', &
'   default integer kind.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   maskr(3)', &
'']

shortname="maskl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('135','maskr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   maskr(3f) - [FORTRAN:INTRINSIC] Right justified mask', &
'', &
'SYNTAX', &
'   result = maskr(i[, kind])', &
'', &
'DESCRIPTION', &
'   maskr(i[, kind]) has its rightmost I bits set to 1, and the', &
'   remaining bits set to 0.', &
'', &
'ARGUMENTS', &
'   I      Shall be of type INTEGER.', &
'   KIND   Shall be a scalar constant expression of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER. If KIND is present, it', &
'   specifies the kind value of the return type; otherwise, it is of the', &
'   default integer kind.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   maskl(3)', &
'', &
'']

shortname="maskr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('136','matmul')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   matmul(3f) - [FORTRAN:INTRINSIC:TRANSFORMATIONAL FUNCTION] matrix', &
'   multiplication', &
'', &
'SYNTAX', &
'   result = matmul(matrix_a, matrix_b)', &
'', &
'DESCRIPTION', &
'   Performs a matrix multiplication on numeric or logical arguments.', &
'', &
'ARGUMENTS', &
'   matrix_a    An array of INTEGER, REAL, COMPLEX, or LOGICAL type,', &
'               with a rank of one or two.', &
'   matrix_b    An array of INTEGER, REAL, or COMPLEX type if matrix_a', &
'               is of a numeric type; otherwise, an array of LOGICAL', &
'               type. The rank shall be one or two, and the first (or', &
'               only) dimension of matrix_b shall be equal to the last', &
'               (or only) dimension of matrix_a.', &
'', &
'RETURN VALUE', &
'   The matrix product of matrix_a and matrix_b. The type and kind of the', &
'   result follow the usual type and kind promotion rules, as for the *', &
'   or .and. operators.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="matmul"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('137','max')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   max(3f) - [FORTRAN:INTRINSIC:NUMERIC] Maximum value of an argument list', &
'', &
'SYNTAX', &
'   result = max(a1, a2 [, a3 [, ...]])', &
'', &
'DESCRIPTION', &
'   Returns the argument with the largest (most positive) value.', &
'', &
'ARGUMENTS', &
'   A1          The type shall be INTEGER or REAL.', &
'   A2,A3,...   An expression of the same type and kind as A1.', &
'', &
'', &
'RETURN VALUE', &
'   The return value corresponds to the maximum value among the arguments,', &
'   and has the same type and kind as the first argument.', &
'', &
'   The function is both elemental and allows for an arbitrary number of', &
'   arguments. This means if some elements are scalar and some are arrays', &
'   that all the arrays must be of the same size, and the returned value', &
'   will be an array that is the result as if multiple calls were made', &
'   with all scalar values with a single element of each array used in', &
'   each call. If called with all arrays the returned array is the same', &
'   as if multiple calls were made with max(arr1(1),arr2(1), ...) to', &
'   max(arr1(N),arr2(N)).', &
'', &
'EXAMPLE', &
'  Sample program', &
'', &
'    program demo_max', &
'    implicit none', &
'    real :: arr1(4)= [10.0,11.0,30.0,-100.0]', &
'    real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]', &
'', &
'       !! this is simple enough because it is not being called elementally', &
'       !! because all arguments are scalar', &
'       !!', &
'', &
'       write(*,*)''scalars:'',max(10.0,11.0,30.0,-100.0)', &
'', &
'       !!', &
'       !! this is all max(3f) could do before it became an elemental', &
'       !! function and is the most intuitive', &
'       !! except that it can take an arbitrary number of options,', &
'       !! which is not common in Fortran without', &
'       !! declaring a lot of optional parameters.', &
'       !!', &
'       !! That is it unless you want to use the elemental features of max(3f)!', &
'', &
'       !! Error: Intrinsic    max    at (1) must have at least two arguments', &
'       !!write(*,*)max(arr1)', &
'       !! This does not work because it is like trying to return', &
'       !! [(max(arr1(i)),i=1,size(arr1))]', &
'       !! so it is trying to take the max of a single value.', &
'       !! To find the largest element of an array', &
'       !! call maxloc(3f) or maxval(3f).', &
'', &
'       !! Error: Different shape for arguments ''a1'' and ''a2'' for intrinsic', &
'       !! ''max'' at (1) on dimension 1 (4 and 5)', &
'       !!write(*,*)max(arr1,arr2)', &
'       !! but this will return an array of', &
'       !! [(max(arr1(N),arr2(N),N=1,size(arr1))]', &
'', &
'       write(*,*)max(arr1,arr2(1:4))', &
'', &
'       !! so this works only if all the arrays are the same size and', &
'       !! you want an array of the largest Nth elements', &
'       !! from the input arrays.', &
'       !! maybe you wanted to do maxval([arr1,arr2]) or', &
'       !! equivalently max(maxval(arr1),maxval(arr2))', &
'       !! to find the single largest element in both arrays?', &
'', &
'       !! compares all scalars to each member of array and', &
'       !! returns array of size arr2', &
'', &
'       write(*,*)''scalars and array:'',max(10.0,11.0,30.0,-100.0,arr2)', &
'', &
'       !! Error: Different shape for arguments ''a5'' and ''a6''', &
'       !! for intrinsic ''max'' at (1) on dimension 1 (5 and 4)', &
'       !! write(*,*)''scalars and array:'',max(10.0,11.0,30.0,-100.0,arr2,arr1)', &
'       !! as the same reason above when arrays are used', &
'       !! (without scalar values) all the arrays must be the same size', &
'', &
'       write(*,*)''scalars and array:'',max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)', &
'', &
'    end program demo_max', &
'', &
'  Results:', &
'', &
'    scalars:   30.000000', &
'      20.0000000  21.000000  32.000000 -100.00000', &
'    scalars and array:   30.000000  30.000000  32.000000  30.000000  2200.0000', &
'    scalars and array:   40.000000  40.000000  40.000000  40.000000', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   maxloc(3), maxval(3), min(3)', &
'']

shortname="max"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('138','maxexponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   maxexponent(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Maximum exponent', &
'   of a real kind', &
'', &
'SYNTAX', &
'   result = maxexponent(x)', &
'', &
'DESCRIPTION', &
'   maxexponent(x) returns the maximum exponent in the model of the', &
'   type of X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the default integer', &
'   kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_maxexponent', &
'    implicit none', &
'      real(kind=4) :: x', &
'      real(kind=8) :: y', &
'', &
'      print *, minexponent(x), maxexponent(x)', &
'      print *, minexponent(y), maxexponent(y)', &
'    end program demo_maxexponent', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="maxexponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('139','maxloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   maxloc(3f) - [FORTRAN:INTRINSIC:ARRAY LOCATION] Location of the', &
'   maximum value within an array', &
'', &
'SYNTAX', &
'   result = maxloc(array, dim [, mask])', &
'   result = maxloc(array [, mask])', &
'', &
'DESCRIPTION', &
'   Determines the location of the element in the array with the maximum', &
'   value, or, if the DIM argument is supplied, determines the', &
'   locations of the maximum element along each row of the array in the', &
'   DIM direction. If MASK is present, only the elements for', &
'   which MASK is .true. are considered. If more than one', &
'   element in the array has the maximum value, the location returned is', &
'   that of the first such element in array element order. If the array has', &
'   zero size, or all of the elements of MASK are .false., then', &
'   the result is an array of zeroes. Similarly, if DIM is supplied', &
'   and all of the elements of MASK along a given row are zero, the', &
'   result value for that row is zero.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array of type INTEGER, REAL, or CHARACTER.', &
'   DIM      (Optional) Shall be a scalar of type', &
'            INTEGER, with a value between one and the rank of ARRAY,', &
'            inclusive. It may not be an optional dummy argument.', &
'   MASK     Shall be an array of type LOGICAL, and conformable with ARRAY.', &
'', &
'RETURN VALUE', &
'   If DIM is absent, the result is a rank-one array with a length', &
'   equal to the rank of ARRAY. If DIM is present, the result', &
'   is an array with a rank one less than the rank of ARRAY, and a', &
'   size corresponding to the size of ARRAY with the DIM', &
'   dimension removed. If DIM is present and ARRAY has a rank', &
'   of one, the result is a scalar. In all cases, the result is of default', &
'   INTEGER type.', &
'', &
'EXAMPLE', &
'  sample program:', &
'', &
'    program demo_maxloc', &
'    implicit none', &
'    integer,save :: ints(3,5)= reshape([&', &
'       1,  2,  3,  4,  5, &', &
'      10, 20, 30, 40, 50, &', &
'      11, 22, 33, 44, 55  &', &
'    ],shape(ints),order=[2,1])', &
'    write(*,*) maxloc(ints)', &
'    write(*,*) maxloc(ints,dim=1)', &
'    write(*,*) maxloc(ints,dim=2)', &
'    end program demo_maxloc', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   max(3), maxval(3)', &
'']

shortname="maxloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('140','maxval')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   maxval(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] determines the', &
'   maximum value in an array or row', &
'', &
'SYNTAX', &
'   result = maxval(array, dim [, mask])', &
'', &
'   result = maxval(array [, mask])', &
'', &
'DESCRIPTION', &
'   Determines the maximum value of the elements in an array value, or,', &
'   if the DIM argument is supplied, determines the maximum value along', &
'   each row of the array in the DIM direction. If MASK is present, only', &
'   the elements for which MASK is .true. are considered. If the array', &
'   has zero size, or all of the elements of MASK are .false., then the', &
'   result is the most negative number of the type and kind of ARRAY if', &
'   ARRAY is numeric, or a string of nulls if ARRAY is of character type.', &
'', &
'ARGUMENTS', &
'   ARRAY   Shall be an array of type INTEGER,', &
'           REAL, or CHARACTER.', &
'   DIM     (Optional) Shall be a scalar of type', &
'           INTEGER, with a value between one and the rank of ARRAY,', &
'           inclusive. It may not be an optional dummy argument.', &
'   MASK    (Optional) Shall be an array of type LOGICAL,', &
'           and conformable with ARRAY.', &
'', &
'RETURN VALUE', &
'   If DIM is absent, or if ARRAY has a rank of one, the result', &
'   is a scalar. If DIM is present, the result is an array with a', &
'   rank one less than the rank of ARRAY, and a size corresponding to', &
'   the size of ARRAY with the DIM dimension removed. In all', &
'   cases, the result is of the same type and kind as ARRAY.', &
'', &
'EXAMPLE', &
'  sample program:', &
'', &
'    program demo_maxval', &
'    implicit none', &
'    integer,save :: ints(3,5)= reshape([&', &
'       1,  2,  3,  4,  5, &', &
'      10, 20, 30, 40, 50, &', &
'      11, 22, 33, 44, 55  &', &
'    ],shape(ints),order=[2,1])', &
'    write(*,*) maxval(ints)', &
'    write(*,*) maxval(ints,dim=1)', &
'    write(*,*) maxval(ints,dim=2)', &
'    ! find biggest number less than 30 with mask', &
'    write(*,*) maxval(ints,mask=ints.lt.30)', &
'    end program demo_maxval', &
'', &
'  Results:', &
'', &
'   55', &
'   11     22     33     44     55', &
'    5     50     55', &
'   22', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   max(3), maxloc(3)', &
'', &
'CATEGORY', &
'', &
'   intrinsics', &
'']

shortname="maxval"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('141','merge')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   merge(3f) - [FORTRAN:INTRINSIC:ARRAY CONSTRUCTION] Merge variables', &
'', &
'SYNTAX', &
'     result = merge(tsource, fsource, mask)', &
'', &
'DESCRIPTION', &
'   Select values from two arrays or scalars according to a logical', &
'   mask. The result is equal to TSOURCE if MASK is .true., or equal to', &
'   FSOURCE if it is .false. .', &
'', &
'ARGUMENTS', &
'   TSOURCE    May be of any type.', &
'   FSOURCE    Shall be of the same type and type parameters as TSOURCE.', &
'   MASK       Shall be of type LOGICAL.', &
'', &
'RETURN VALUE', &
'   The result is of the same type and type parameters as TSOURCE. For any', &
'   element the result is TSOURCE if MASK is true and FSOURCE otherwise.', &
'', &
'EXAMPLES', &
'  The value of', &
'', &
'   merge (1.0, 0.0, k > 0)', &
'', &
'  is 1.0 for K=5 and 0.0 for K=2.', &
'', &
'  Note that (currently) CHARACTER values must be of the same length.', &
'', &
'   program demo_merge', &
'   implicit none', &
'   integer :: tsrc(2,3), fsrc(2,3), answer(2,3)', &
'   logical :: mask(2,3)', &
'   integer :: i', &
'      tsrc(1,:)=[ 1,6,5 ]; fsrc(1,:)=[ 0,3,2 ]; &', &
'      & mask(1,:)=[.true., .false.,.true.]', &
'      tsrc(2,:)=[ 2,4,6 ]; fsrc(2,:)=[ 7,4,8 ]; &', &
'      & mask(2,:)=[.false.,.false.,.true.]', &
'      answer=merge(tsrc,fsrc,mask)', &
'      write(*,''(3i2)'')(answer(i,:),i=1,size(answer,dim=1))', &
'   end program demo_merge', &
'', &
'  Expected result', &
'', &
'    > 1 3 5', &
'    > 7 4 6', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="merge"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('142','merge_bits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   merge_bits(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Merge of bits', &
'   under mask', &
'', &
'SYNTAX', &
'   result = merge_bits(i, j, mask)', &
'', &
'DESCRIPTION', &
'   merge_bits(i, j, mask) merges the bits of I and J as determined by', &
'   the mask. The k-th bit of the result is equal to the k-th bit of', &
'   I if the k-th bit of MASK is 1; it is equal to the k-th bit of', &
'   J otherwise.', &
'', &
'ARGUMENTS', &
'  I      Shall be of type INTEGER.', &
'  J      Shall be of type INTEGER and of the same kind as I.', &
'  MASK   Shall be of type INTEGER and of the same kind as I.', &
'', &
'RETURN VALUE', &
'   The result is of the same type and kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'']

shortname="merge_bits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('143','min')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   min(3f) - [FORTRAN:INTRINSIC:NUMERIC] Minimum value of an argument list', &
'', &
'SYNTAX', &
'   result = min(a1, a2 [, a3, ...])', &
'', &
'DESCRIPTION', &
'   Returns the argument with the smallest (most negative) value.', &
'', &
'ARGUMENTS', &
'   A1           The type shall be INTEGER or REAL.', &
'   A2, A3, ...  An expression of the same type and kind as A1.', &
'', &
'RETURN VALUE', &
'   The return value corresponds to the minimum value among the arguments,', &
'   and has the same type and kind as the first argument.', &
'', &
'EXAMPLE', &
'  Sample program', &
'', &
'    program demo_min', &
'    implicit none', &
'    write(*,*)min(10.0,11.0,30.0,-100.0)', &
'    end program demo_min', &
'', &
'  Results:', &
'', &
'      -100.0000000', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   max(3), minloc(3), minval(3)', &
'']

shortname="min"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('144','minexponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   minexponent(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Minimum exponent', &
'   of a real kind', &
'', &
'SYNTAX', &
'   result = minexponent(x)', &
'', &
'DESCRIPTION', &
'   minexponent(x) returns the minimum exponent in the model of the', &
'   type of X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the default integer', &
'   kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_minexponent', &
'    implicit none', &
'    real(kind=4) :: x', &
'    real(kind=8) :: y', &
'       print *, minexponent(x), maxexponent(x)', &
'       print *, minexponent(y), maxexponent(y)', &
'    end program demo_minexponent', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'', &
'']

shortname="minexponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('145','minloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   minloc(3f) - [FORTRAN:INTRINSIC:ARRAY LOCATION] Location of the', &
'   minimum value within an array', &
'', &
'SYNTAX', &
'   result = minloc(array, dim [, mask])', &
'   result = minloc(array [, mask])', &
'', &
'DESCRIPTION', &
'   Determines the location of the element in the array with the', &
'   minimum value, or, if the DIM argument is supplied, determines the', &
'   locations of the minimum element along each row of the array in the', &
'   DIM direction. If MASK is present, only the elements for which MASK', &
'   is .true. are considered. If more than one element in the array has', &
'   the minimum value, the location returned is that of the first such', &
'   element in array element order. If the array has zero size, or all', &
'   of the elements of MASK are .false., then the result is an array of', &
'   zeroes. Similarly, if DIM is supplied and all of the elements of MASK', &
'   along a given row are zero, the result value for that row is zero.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array of type INTEGER, REAL, or CHARACTER.', &
'   DIM      (Optional) Shall be a scalar of type', &
'            INTEGER, with a value between one and the rank of ARRAY,', &
'            inclusive. It may not be an optional dummy argument.', &
'   MASK     Shall be an array of type LOGICAL,', &
'            and conformable with ARRAY.', &
'', &
'RETURN VALUE', &
'   If DIM is absent, the result is a rank-one array with a length equal', &
'   to the rank of ARRAY. If DIM is present, the result is an array with', &
'   a rank one less than the rank of ARRAY, and a size corresponding to', &
'   the size of ARRAY with the DIM dimension removed. If DIM is present', &
'   and ARRAY has a rank of one, the result is a scalar. In all cases,', &
'   the result is of default INTEGER type.', &
'', &
'EXAMPLE', &
'  sample program:', &
'', &
'    program demo_minloc', &
'    implicit none', &
'    integer,save :: ints(3,5)= reshape([&', &
'       4, 10,  1,  7, 13, &', &
'       9, 15,  6, 12,  3, &', &
'      14,  5, 11,  2,  8  &', &
'    ],shape(ints),order=[2,1])', &
'    write(*,*) minloc(ints)', &
'    write(*,*) minloc(ints,dim=1)', &
'    write(*,*) minloc(ints,dim=2)', &
'    ! where in each column is the smallest number .gt. 10 ?', &
'    write(*,*) minloc(ints,dim=2,mask=ints.gt.10)', &
'    ! a one-dimensional array with dim=1 explicitly listed returns a scalar', &
'    write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar', &
'    end program demo_minloc', &
'', &
'  Results:', &
'', &
'      1  3        ', &
'      1  3  1  3  2', &
'      3  5  4     ', &
'      5  4  3     ', &
'      7           ', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   min(3), minval(3)', &
'']

shortname="minloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('146','minval')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   minval(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] Minimum value of an array', &
'', &
'SYNTAX', &
'   result = minval(array, dim [, mask])', &
'   result = minval(array [, mask])', &
'', &
'DESCRIPTION', &
'   Determines the minimum value of the elements in an array value, or, if', &
'   the DIM argument is supplied, determines the minimum value along', &
'   each row of the array in the DIM direction. If MASK is', &
'   present, only the elements for which MASK is .true. are', &
'   considered. If the array has zero size, or all of the elements of', &
'   MASK are .false., then the result is huge(array) if', &
'   ARRAY is numeric, or a string of char(255) characters if', &
'   ARRAY is of character type.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array of type INTEGER,', &
'            REAL, or CHARACTER.', &
'   DIM      (Optional) Shall be a scalar of type', &
'            INTEGER, with a value between one and the rank of ARRAY,', &
'            inclusive. It may not be an optional dummy argument.', &
'   MASK     Shall be an array of type LOGICAL,', &
'            and conformable with ARRAY.', &
'', &
'RETURN VALUE', &
'   If DIM is absent, or if ARRAY has a rank of one, the result', &
'   is a scalar. If DIM is present, the result is an array with a', &
'   rank one less than the rank of ARRAY, and a size corresponding to', &
'   the size of ARRAY with the DIM dimension removed. In all', &
'   cases, the result is of the same type and kind as ARRAY.', &
'', &
'EXAMPLE', &
'  sample program:', &
'', &
'    program demo_minval', &
'    implicit none', &
'    integer,save :: ints(3,5)= reshape([&', &
'       1,  2,  3,  4,  5, &', &
'      10, 20, 30, 40, 50, &', &
'      11, 22, 33, 44, 55  &', &
'    ],shape(ints),order=[2,1])', &
'    write(*,*) minval(ints)', &
'    write(*,*) minval(ints,dim=1)', &
'    write(*,*) minval(ints,dim=2)', &
'    end program demo_minval', &
'', &
'  results:', &
'', &
'    > 1', &
'    > 1    2     3     4     5', &
'    > 1   10    11', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   min(3), minloc(3)', &
'']

shortname="minval"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('147','mod')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   mod(3f) - [FORTRAN:INTRINSIC:NUMERIC] Remainder function', &
'', &
'SYNTAX', &
'   result = mod(a, p)', &
'', &
'DESCRIPTION', &
'   mod(a,p) computes the remainder of the division of A by P.', &
'', &
'ARGUMENTS', &
'   A    Shall be a scalar of type INTEGER or REAL.', &
'   P    Shall be a scalar of the same type and kind as A and not equal to zero.', &
'', &
'RETURN VALUE', &
'   The return value is the result of a - (int(a/p) * p). The type', &
'   and kind of the return value is the same as that of the arguments. The', &
'   returned value has the same sign as A and a magnitude less than the', &
'   magnitude of P.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_mod', &
'   implicit none', &
'     print *, mod(17,3)           ! yields 2', &
'     print *, mod(17.5,5.5)       ! yields 1.0', &
'     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0', &
'     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0', &
'', &
'     print *, mod(-17,3)          ! yields -2', &
'     print *, mod(-17.5,5.5)      ! yields -1.0', &
'     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0', &
'     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0', &
'', &
'     print *, mod(17,-3)          ! yields 2', &
'     print *, mod(17.5,-5.5)      ! yields 1.0', &
'     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0', &
'     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0', &
'   end program demo_mod', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   modulo(3)', &
'']

shortname="mod"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('148','modulo')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   modulo(3f) - [FORTRAN:INTRINSIC:NUMERIC] Modulo function', &
'', &
'SYNTAX', &
'   result = modulo(a, p)', &
'', &
'DESCRIPTION', &
'   modulo(a,p) computes the A modulo P.', &
'', &
'ARGUMENTS', &
'  A    Shall be a scalar of type INTEGER or REAL.', &
'  P    Shall be a scalar of the same type and kind as A. It shall', &
'  not be zero.', &
'', &
'RETURN VALUE', &
'   The type and kind of the result are those of the arguments.', &
'', &
'   * If A and P are of type INTEGER:', &
'     modulo(a,p) has the value of a - floor (real(a) / real(p)) * p.', &
'   * If A and P are of type REAL:', &
'     modulo(a,p) has the value of a - floor (a / p) * p.', &
'', &
'   The returned value has the same sign as P and a magnitude less than', &
'   the magnitude of P.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_modulo', &
'   implicit none', &
'     print *, modulo(17,3)        ! yields 2', &
'     print *, modulo(17.5,5.5)    ! yields 1.0', &
'', &
'     print *, modulo(-17,3)       ! yields 1', &
'     print *, modulo(-17.5,5.5)   ! yields 4.5', &
'', &
'     print *, modulo(17,-3)       ! yields -1', &
'     print *, modulo(17.5,-5.5)   ! yields -4.5', &
'   end program demo_modulo', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   mod(3)', &
'']

shortname="modulo"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('149','move_alloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   move_alloc(3f) - [FORTRAN:INTRINSIC] Move allocation from one object', &
'   to another', &
'', &
'SYNTAX', &
'   call move_alloc(src, dest)', &
'', &
'DESCRIPTION', &
'   move_alloc(src, dest) moves the allocation from SRC to DEST.', &
'   SRC will become deallocated in the process.', &
'', &
'ARGUMENTS', &
'   SRC    ALLOCATABLE, intent(inout), may be of any type and kind.', &
'   DEST   ALLOCATABLE, intent(out), shall be of the same type, kind and', &
'          rank as SRC.', &
'', &
'EXAMPLE', &
'  Basic Sample program to allocate a bigger grid', &
'', &
'   program demo_move_alloc', &
'   implicit none', &
'   ! Example to allocate a bigger GRID', &
'   real, allocatable :: grid(:), tempgrid(:)', &
'   integer :: n, i', &
'', &
'      ! initialize small GRID', &
'      n = 3', &
'      allocate (grid(1:n))', &
'      grid = [ (real (i), i=1,n) ]', &
'', &
'      ! initialize TEMPGRID which will be used to replace GRID', &
'      allocate (tempgrid(1:2*n))    ! Allocate bigger grid', &
'      tempgrid(::2)  = grid         ! Distribute values to new locations', &
'      tempgrid(2::2) = grid + 0.5   ! initialize other values', &
'', &
'      ! move TEMPGRID to GRID', &
'      call MOVE_ALLOC (from=tempgrid, to=grid)', &
'', &
'      ! TEMPGRID should no longer be allocated', &
'      ! and GRID should be the size TEMPGRID was', &
'      if (size (grid) /= 2*n .or. allocated (tempgrid)) then', &
'         print *, "Failure in move_alloc!"', &
'      endif', &
'      print *, allocated(grid), allocated(tempgrid)', &
'      print ''(99f8.3)'', grid', &
'', &
'   end program demo_move_alloc', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Pure subroutine', &
'', &
'SEE ALSO', &
'   allocated(3)', &
'']

shortname="move_alloc"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('150','mvbits')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   mvbits(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Move bits from one', &
'   integer to another', &
'', &
'SYNTAX', &
'   call mvbits(from, frompos, len, to, topos)', &
'', &
'DESCRIPTION', &
'   Moves LEN bits from positions FROMPOS through', &
'   frompos+len-1 of FROM to positions TOPOS through', &
'   topos+len-1 of TO. The portion of argument TO not', &
'   affected by the movement of bits is unchanged. The values of', &
'   frompos+len-1 and topos+len-1 must be less than', &
'   bit_size(from).', &
'', &
'ARGUMENTS', &
'   FROM      The type shall be INTEGER.', &
'   FROMPOS   The type shall be INTEGER.', &
'   LEN       The type shall be INTEGER.', &
'   TO        The type shall be INTEGER, of the', &
'             same kind as FROM.', &
'   TOPOS     The type shall be INTEGER.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental subroutine]]', &
'', &
'SEE ALSO', &
'   ibclr(3), ibset(3), ibits(3),', &
'   iand(3), ior(3)', &
'']

shortname="mvbits"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('151','nearest')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   nearest(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Nearest', &
'   representable number', &
'', &
'SYNTAX', &
'   result = nearest(x, s)', &
'', &
'DESCRIPTION', &
'   nearest(x, s) returns the processor-representable number nearest', &
'   to X in the direction indicated by the sign of S.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'   S    Shall be of type REAL and not equal to zero.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type as X. If S is positive,', &
'   NEAREST returns the processor-representable number greater than X', &
'   and nearest to it. If S is negative, NEAREST returns the', &
'   processor-representable number smaller than X and nearest to it.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_nearest', &
'   implicit none', &
'     real :: x, y', &
'     x = nearest(42.0, 1.0)', &
'     y = nearest(42.0, -1.0)', &
'     write (*,"(3(g20.15))") x, y, x - y', &
'   end program demo_nearest', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="nearest"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('152','new_line')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   new_line(3f) - [FORTRAN:INTRINSIC:CHARACTER] New line character', &
'', &
'SYNTAX', &
'   result = new_line(c)', &
'', &
'DESCRIPTION', &
'   new_line(c) returns the new-line character.', &
'', &
'ARGUMENTS', &
'   C  - The argument shall be a scalar or array of the type CHARACTER.', &
'', &
'RETURN VALUE', &
'   Returns a CHARACTER scalar of length one with the new-line character of', &
'   the same kind as parameter C.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_new_line', &
'      implicit none', &
'      write(*,''(A)'') ''This is record 1.''//NEW_LINE(''A'')//''This is record 2.''', &
'    end program demo_new_line', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="new_line"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('153','nint')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   nint(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Nearest whole number', &
'', &
'SYNTAX', &
'    elemental function nint(x [, kind=NN]) result(ANSWER)', &
'', &
'     real(kind=??),intent(in) :: X', &
'     integer(kind=NN) :: ANSWER', &
'', &
'DESCRIPTION', &
'   NINT(X) rounds its argument to the nearest whole number with its', &
'   sign preserved.', &
'', &
'   The user must ensure the value is a valid value for the range of the', &
'   KIND returned. If the processor cannot represent the result in the', &
'   kind specified, the result is undefined.', &
'', &
'   If X is greater than zero, NINT(X) has the value INT(X+0.5).', &
'', &
'   If X is less than or equal to zero, NINT(X) has the value INT(a-0.5).', &
'', &
'ARGUMENTS', &
'   X       The type of the argument shall be REAL.', &
'', &
'   KIND    (Optional) A constant INTEGER expression indicating', &
'           the kind parameter of the result. Otherwise, the kind type', &
'           parameter is that of default INTEGER type.', &
'', &
'RETURN VALUE', &
'', &
'  ANSWER   The result is the integer nearest X, or if there are two', &
'           integers equally near X, the result is whichever such integer', &
'           has the greater magnitude.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_nint ', &
'    implicit none', &
'    integer,parameter :: dp=kind(0.0d0)', &
'    real              :: x4 = 1.234E0', &
'    real(kind=dp)     :: x8 = 4.721_dp', &
'', &
'    ! basic use', &
'       print *, nint(x4), nint(x8),nint(-x8)', &
'', &
'    ! issues', &
'    ISSUES: block', &
'    use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'    integer :: icheck', &
'       ! make sure input is in range for the type returned', &
'       write(*,*)''Range limits for typical KINDS:''', &
'       write(*,''(1x,g0,1x,g0)'')  &', &
'       & int8,huge(0_int8),   &', &
'       & int16,huge(0_int16), &', &
'       & int32,huge(0_int32), &', &
'       & int64,huge(0_int64)', &
'', &
'       ! the standard does not require this to be an error ...', &
'       x8=12345.67e15 ! too big of a number', &
'       icheck=selected_int_kind(ceiling(log10(x8)))', &
'       write(*,*)''Any KIND big enough? ICHECK='',icheck', &
'       print *, ''These are all wrong answers for '',x8', &
'       print *, nint(x8,kind=int8)', &
'       print *, nint(x8,kind=int16)', &
'       print *, nint(x8,kind=int32)', &
'       print *, nint(x8,kind=int64)', &
'    endblock ISSUES', &
'', &
'    end program demo_nint', &
'', &
'  Results', &
'', &
'     > 1 5 -5', &
'     > Range limits for typical KINDS:', &
'     > 1 127', &
'     > 2 32767', &
'     > 4 2147483647', &
'     > 8 9223372036854775807', &
'     > Any KIND big enough? ICHECK=          -1', &
'     > These are all wrong answers for   1.234566949990144E+019', &
'     > 0', &
'     > 0', &
'     > -2147483648', &
'     > -9223372036854775808', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, with KIND argument [[Fortran 90]] and later', &
'', &
'SEE ALSO', &
'   ceiling(3), floor(3)', &
' JSU', &
'']

shortname="nint"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('154','norm2')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   norm2(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Euclidean vector norm', &
'', &
'SYNTAX', &
'   result = norm2(array[, dim])', &
'', &
'DESCRIPTION', &
'   Calculates the Euclidean vector norm (L_2 norm) of ARRAY along dimension DIM.', &
'', &
'ARGUMENTS', &
'  ARRAY    Shall be an array of type REAL.', &
'  DIM      (Optional) shall be a scalar of type INTEGER with a value', &
'           in the range from 1 to n, where n equals the rank of ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as ARRAY.', &
'', &
'   If DIM is absent, a scalar with the square root of the sum of', &
'   squares of the elements of ARRAY is returned. Otherwise, an array of', &
'   rank n-1, where "n" equals the rank of ARRAY, and a shape similar', &
'   to that of ARRAY with dimension DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_norm2', &
'   implicit none', &
'     real :: x(5) = [ real :: 1, 2, 3, 4, 5 ]', &
'     print *, norm2(x)  ! = sqrt(55.) ~ 7.416', &
'   end program demo_norm2', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   product(3), sum(3), hypot(3)', &
'']

shortname="norm2"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('155','not')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   not(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Logical negation', &
'', &
'SYNTAX', &
'   result = not(i)', &
'', &
'DESCRIPTION', &
'   NOT returns the bitwise Boolean inverse of I.', &
'', &
'ARGUMENTS', &
'   I    The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return type is INTEGER, of the same kind as the argument.', &
'EXAMPLE', &
'  Sample program', &
'', &
'    program demo_not', &
'    implicit none', &
'    integer :: i', &
'       i=13741', &
'       write(*,''(b32.32,1x,i0)'')i,i', &
'       write(*,''(b32.32,1x,i0)'')not(i),not(i)', &
'    end program demo_not', &
'', &
'  Results:', &
'', &
'   00000000000000000011010110101101 13741', &
'   11111111111111111100101001010010 -13742', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   iand(3), ior(3), ieor(3), ibits(3),', &
'   ibset(3), ibclr(3)', &
' JSU', &
'']

shortname="not"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('156','null')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   null(3f) - [FORTRAN:INTRINSIC:TRANSFORMATIONAL FUNCTION] Function', &
'   that returns a disassociated pointer', &
'', &
'SYNTAX', &
'    ptr => null([mold])', &
'', &
'DESCRIPTION', &
'   Returns a disassociated pointer.', &
'', &
'   If MOLD is present, a disassociated pointer of the same type is', &
'   returned, otherwise the type is determined by context.', &
'', &
'   In [[Fortran 95]], MOLD is optional. Please note that [[Fortran 2003]]', &
'   includes cases where it is required.', &
'', &
'ARGUMENTS', &
'   MOLD    (Optional) shall be a pointer of any association', &
'           status and of any type.', &
'', &
'RETURN VALUE', &
'   A disassociated pointer.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    real, pointer, dimension(:) :: vec => null ()', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   associated(3)', &
'']

shortname="null"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('157','num_images')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   num_images(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Number of images', &
'', &
'SYNTAX', &
'   result = num_images(distance, failed)', &
'', &
'DESCRIPTION', &
'   Returns the number of images.', &
'', &
'ARGUMENTS', &
'   DISTANCE   (optional, intent(in)) Nonnegative scalar integer', &
'   FAILED     (optional, intent(in)) Scalar logical expression', &
'', &
'RETURN VALUE', &
'   Scalar default-kind integer. If DISTANCE is not present or has', &
'   value 0, the number of images in the current team is returned. For', &
'   values smaller or equal distance to the initial team, it returns the', &
'   number of images index on the ancestor team which has a distance of', &
'   DISTANCE from the invoking team. If DISTANCE is larger than the', &
'   distance to the initial team, the number of images of the initial team', &
'   is returned. If FAILED is not present the total number of images is', &
'   returned; if it has the value .true., the number of failed images is', &
'   returned, otherwise, the number of images which do have not the failed', &
'   status.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_num_images', &
'   implicit none', &
'   integer :: value[*]', &
'   integer :: i', &
'   value = this_image()', &
'      sync all', &
'      if (this_image() == 1) then', &
'        do i = 1, num_images()', &
'          write(*,''(2(a,i0))'') ''value['', i, ''] is '', value[i]', &
'        end do', &
'      endif', &
'   end program demo_num_images', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later. With DISTANCE or FAILED argument,', &
'   [[TS 18508]] or later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   this_image(3), image_index(3)', &
'']

shortname="num_images"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('158','pack')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   pack(3f) - [FORTRAN:INTRINSIC:ARRAY CONSTRUCTION] Pack an array into', &
'   an array of rank one', &
'', &
'SYNTAX', &
'   result = pack(array, mask[,vector])', &
'', &
'DESCRIPTION', &
'   Stores the elements of ARRAY in an array of rank one.', &
'', &
'   The beginning of the resulting array is made up of elements whose MASK', &
'   equals TRUE. Afterwards, positions are filled with elements taken from', &
'   VECTOR.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array of any type.', &
'   MASK     Shall be an array of type LOGICAL and of the same size', &
'            as ARRAY. Alternatively, it may be a LOGICAL scalar.', &
'   VECTOR   (Optional) shall be an array of the same type as ARRAY', &
'            and of rank one. If present, the number of elements in VECTOR', &
'            shall be equal to or greater than the number of true elements in', &
'            MASK. If MASK is scalar, the number of elements in VECTOR', &
'            shall be equal to or greater than the number of elements in', &
'            ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is an array of rank one and the same type as that of ARRAY.', &
'   If VECTOR is present, the result size is that of VECTOR, the', &
'   number of TRUE values in MASK otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_pack', &
'    implicit none', &
'    call test1()', &
'    call test2()', &
'    call test3()', &
'    contains', &
'    !', &
'    subroutine test1()', &
'    ! gathering nonzero elements from an array:', &
'    integer :: m(6)', &
'      m = [ 1, 0, 0, 0, 5, 0 ]', &
'      write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0)  ! "1 5"', &
'    end subroutine test1', &
'    !', &
'    subroutine test2()', &
'    ! Gathering nonzero elements from an array and appending elements', &
'    ! from VECTOR:', &
'    integer :: m(4)', &
'      m = [ 1, 0, 0, 2 ]', &
'      write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0, [ 0, 0, 3, 4 ])  ! "1 2 3 4"', &
'    end subroutine test2', &
'    !', &
'    subroutine test3()', &
'    ! select strings whose second character is "a"', &
'    character(len=10) :: m(4)', &
'    m = [ character(len=10) :: ''ape'', ''bat'', ''cat'', ''dog'']', &
'      write(*, fmt="(*(g0, '' ''))") pack(m, m(:)(2:2) == ''a'' )  ! "bat" "cat"', &
'    end subroutine test3', &
'    !', &
'    end program demo_pack', &
'', &
'  Results:', &
'', &
'   1 5', &
'   1 2 3 4', &
'   bat        cat', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   unpack(3)', &
'', &
'']

shortname="pack"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('159','parity')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   parity(3f) - [FORTRAN:INTRINSIC:TRANSFORMATIONAL FUNCTION] Reduction', &
'   with exclusive OR()', &
'', &
'SYNTAX', &
'   result = parity(mask[, dim])', &
'', &
'DESCRIPTION', &
'   Calculates the parity (i.e. the reduction using .xor.) of MASK along', &
'   dimension DIM.', &
'', &
'ARGUMENTS', &
'   MASK    Shall be an array of type LOGICAL.', &
'   DIM     (Optional) shall be a scalar of type INTEGER with a value', &
'           in the range from 1 to n, where n equals the rank of ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as MASK.', &
'', &
'   If DIM is absent, a scalar with the parity of all elements in MASK', &
'   is returned: .true. if an odd number of elements are .true. and', &
'   .false. otherwise. If DIM is present, an array of rank n-1,', &
'   where "n" equals the rank of MASK, and a shape similar to that of', &
'   MASK with dimension DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_parity', &
'   implicit none', &
'     logical :: x(2) = [ .true., .false. ]', &
'     print *, parity(x) ! T', &
'   end program demo_parity', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="parity"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('160','popcnt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   popcnt(3f) - [FORTRAN:INTRINSIC:BIT INQUIRY] Number of bits set', &
'', &
'SYNTAX', &
'   result = popcnt(i)', &
'', &
'DESCRIPTION', &
'   Returns the number of bits set in the binary representation of an integer.', &
'', &
'ARGUMENTS', &
'   I    Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type ''integer'' and of the default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_popcnt', &
'   use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'   & int8, int16, int32, int64', &
'   implicit none', &
'     print *, popcnt(127),       poppar(127)', &
'     print *, popcnt(huge(0)), poppar(huge(0))', &
'     print *, popcnt(huge(0_int8)), poppar(huge(0_int8))', &
'     print *, popcnt(huge(0_int16)), poppar(huge(0_int16))', &
'     print *, popcnt(huge(0_int32)), poppar(huge(0_int32))', &
'     print *, popcnt(huge(0_int64)), poppar(huge(0_int64))', &
'   end program demo_popcnt', &
'', &
'  Sample output:', &
'', &
'       7           1', &
'      31           1', &
'       7           1', &
'      15           1', &
'      31           1', &
'      63           1', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   poppar(3), leadz(3), trailz(3)', &
'']

shortname="popcnt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('161','poppar')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   poppar(3f) - [FORTRAN:INTRINSIC:BIT INQUIRY] Parity of the number of', &
'   bits set', &
'', &
'SYNTAX', &
'   result = poppar(i)', &
'', &
'DESCRIPTION', &
'   Returns the parity of an integer''s binary representation (i.e., the', &
'   parity of the number of bits set).', &
'', &
'ARGUMENTS', &
'   I    Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type ''integer'' and of the default integer kind.', &
'   It is equal to 0 if I has an even number of bits set and 1 if an odd', &
'   number of bits are set.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_popcnt', &
'   use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'   & int8, int16, int32, int64', &
'   implicit none', &
'      print  *,  popcnt(127),            poppar(127)', &
'      print  *,  popcnt(huge(0_int8)),   poppar(huge(0_int8))', &
'      print  *,  popcnt(huge(0_int16)),  poppar(huge(0_int16))', &
'      print  *,  popcnt(huge(0_int32)),  poppar(huge(0_int32))', &
'      print  *,  popcnt(huge(0_int64)),  poppar(huge(0_int64))', &
'   end program demo_popcnt', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   popcnt(3), leadz(3), trailz(3)', &
'']

shortname="poppar"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('162','precision')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   precision(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Decimal precision', &
'   of a real kind', &
'', &
'SYNTAX', &
'    result = precision(x)', &
'', &
'DESCRIPTION', &
'   precision(x) returns the decimal precision in the model of the', &
'   type of X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the default integer', &
'   kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_precision', &
'    implicit none', &
'      real(kind=4) :: x(2)', &
'      complex(kind=8) :: y', &
'', &
'      print *, precision(x), range(x)', &
'      print *, precision(y), range(y)', &
'    end program demo_precision', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   selected_real_kind(3), range(3)', &
'']

shortname="precision"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('163','present')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   present(3f) - [FORTRAN:INTRINSIC] Determine whether an optional dummy', &
'   argument is specified', &
'', &
'SYNTAX', &
'   result = present(a)', &
'', &
'DESCRIPTION', &
'   Determines whether an optional dummy argument is present.', &
'', &
'ARGUMENTS', &
'   A    May be of any type and may be a pointer, scalar or array', &
'        value, or a dummy procedure. It shall be the name of an optional', &
'        dummy argument accessible within the current subroutine or', &
'        function.', &
'', &
'RETURN VALUE', &
'   Returns either TRUE if the optional argument A is present, or', &
'   FALSE otherwise.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_present', &
'    implicit none', &
'      write(*,*) f(), f(42)      ! "f t"', &
'    contains', &
'      logical function f(x)', &
'        integer, intent(in), optional :: x', &
'        f = present(x)', &
'      end function', &
'    end program demo_present', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="present"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('164','product')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   product(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] Product of array elements', &
'', &
'SYNTAX', &
'   * result = product(array[, mask])', &
'   * result = product(array, dim[, mask])', &
'', &
'DESCRIPTION', &
'   Multiplies the elements of ARRAY along dimension DIM if', &
'   the corresponding element in MASK is TRUE.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array of type INTEGER, REAL or COMPLEX.', &
'   DIM      (Optional) shall be a scalar of type INTEGER with a', &
'            value in the range from 1 to n, where n equals the rank of ARRAY.', &
'   MASK     (Optional) shall be of type LOGICAL', &
'            and either be a scalar or an array of the same shape as ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as ARRAY.', &
'', &
'   If DIM is absent, a scalar with the product of all elements in', &
'   ARRAY is returned. Otherwise, an array of rank n-1, where n equals', &
'   the rank of ARRAY, and a shape similar to that of ARRAY with', &
'   dimension DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_product', &
'    implicit none', &
'      integer :: x(5) = [ 1, 2, 3, 4 ,5 ]', &
'      print *, product(x)                    ! all elements, product = 120', &
'      print *, product(x, mask=mod(x, 2)==1) ! odd elements, product = 15', &
'    end program demo_product', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   sum(3), note that an element by element multiplication is done directly', &
'   using the star character.', &
'']

shortname="product"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('165','radix')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   radix(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Base of a model number', &
'', &
'SYNTAX', &
'    result = radix(x)', &
'', &
'DESCRIPTION', &
'   radix(x) returns the base of the model representing the entity X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type INTEGER or REAL', &
'', &
'RETURN VALUE', &
'   The return value is a scalar of type INTEGER and of the default', &
'   integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_radix', &
'    implicit none', &
'      print *, "The radix for the default integer kind is", radix(0)', &
'      print *, "The radix for the default real kind is", radix(0.0)', &
'    end program demo_radix', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   scale(3), selected_real_kind(3)', &
'']

shortname="radix"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('166','random_number')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   random_number(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:RANDOM]', &
'   Pseudo-random number', &
'', &
'SYNTAX', &
'   random_number(harvest)', &
'', &
'DESCRIPTION', &
'   Returns a single pseudorandom number or an array of pseudorandom numbers', &
'   from the uniform distribution over the range 0 <= x < 1.', &
'', &
'ARGUMENTS', &
'   HARVEST    Shall be a scalar or an array of type REAL.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_random_number', &
'   use, intrinsic :: iso_fortran_env, only : dp=>real64', &
'   implicit none', &
'   integer, allocatable :: seed(:)', &
'   integer              :: n', &
'   integer              :: first,last', &
'   integer              :: i', &
'   integer              :: rand_int', &
'   integer,allocatable  :: count(:)', &
'   real(kind=dp)        :: rand_val', &
'   call random_seed(size = n)', &
'   allocate(seed(n))', &
'   call random_seed(get=seed)', &
'   first=1', &
'   last=10', &
'   allocate(count(last-first+1))', &
'   ! To have a discrete uniform distribution on the integers {first, first+1,', &
'   ! ..., last-1, last} carve the continuous distribution up into last+1-first', &
'   ! equal sized chunks, mapping each chunk to an integer.', &
'   !', &
'   ! One way is:', &
'   !   call random_number(rand_val)', &
'   ! choose one from last-first+1 integers', &
'   !   rand_int = first + FLOOR((last+1-first)*rand_val)', &
'      count=0', &
'   ! generate a lot of random integers from 1 to 10 and count them.', &
'   ! with a large number of values you should get about the same number', &
'   ! of each value', &
'      do i=1,100000000', &
'         call random_number(rand_val)', &
'         rand_int=first+floor((last+1-first)*rand_val)', &
'         if(rand_int.ge.first.and.rand_int.le.last)then', &
'            count(rand_int)=count(rand_int)+1', &
'         else', &
'            write(*,*)rand_int,'' is out of range''', &
'         endif', &
'      enddo', &
'      write(*,''(i0,1x,i0)'')(i,count(i),i=1,size(count))', &
'   end program demo_random_number', &
'', &
'  Sample output:', &
'', &
'   >1 10003588', &
'   >2 10000104', &
'   >3 10000169', &
'   >4 9997996', &
'   >5 9995349', &
'   >6 10001304', &
'   >7 10001909', &
'   >8 9999133', &
'   >9 10000252', &
'   >10 10000196', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   random_seed(3)', &
'']

shortname="random_number"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('167','random_seed')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   random_seed(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:RANDOM] Initialize', &
'   a pseudo-random number sequence', &
'', &
'SYNTAX', &
'   call random_seed([size, put, get])', &
'', &
'DESCRIPTION', &
'   Restarts or queries the state of the pseudorandom number generator', &
'   used by random_number.', &
'', &
'   If random_seed is called without arguments, it is seeded with random', &
'   data retrieved from the operating system.', &
'', &
'ARGUMENTS', &
'   SIZE  (Optional) Shall be a scalar and of type default', &
'         INTEGER, with intent(out). It specifies the minimum size of', &
'         the arrays used with the PUT and GET arguments.', &
'', &
'   PUT   (Optional) Shall be an array of type default INTEGER and', &
'         rank one. It is intent(in) and the size of the array must be', &
'         larger than or equal to the number returned by the SIZE argument.', &
'', &
'   GET   (Optional) Shall be an array of type default INTEGER and', &
'         rank one. It is intent(out) and the size of the array must be', &
'         larger than or equal to the number returned by the SIZE argument.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_random_seed', &
'     implicit none', &
'     integer, allocatable :: seed(:)', &
'     integer :: n', &
'', &
'     call random_seed(size = n)', &
'     allocate(seed(n))', &
'     call random_seed(get=seed)', &
'     write (*, *) seed', &
'   end program demo_random_seed', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   random_number(3)', &
'']

shortname="random_seed"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('168','range')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   range(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Decimal exponent range', &
'   of a real kind', &
'', &
'SYNTAX', &
'   result = range(x)', &
'', &
'DESCRIPTION', &
'   range(x) returns the decimal exponent range in the model of the', &
'   type of X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the default integer', &
'   kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_range', &
'    implicit none', &
'    real(kind=4) :: x(2)', &
'    complex(kind=8) :: y', &
'       print *, precision(x), range(x)', &
'       print *, precision(y), range(y)', &
'    end program demo_range', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   selected_real_kind(3), precision(3)', &
'', &
'']

shortname="range"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('169','rank')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   rank(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] Rank of a data object', &
'', &
'SYNTAX', &
'   result = rank(a)', &
'DESCRIPTION', &
'   rank(a) returns the rank of a scalar or array data object.', &
'ARGUMENTS', &
'   A    can be of any type', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the default integer', &
'   kind. For arrays, their rank is returned; for scalars zero is', &
'   returned.', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_rank', &
'   implicit none', &
'     integer :: a', &
'     real, allocatable :: b(:,:)', &
'     real  :: c(10,20,30)', &
'     print *, rank(a), rank(b), rank(c)', &
'   end program demo_rank', &
'', &
'  Results:', &
'     0           2           3', &
'', &
'  Expected output:', &
'', &
'STANDARD', &
'   [[TS 29113]]', &
'CLASS', &
'   Inquiry function', &
'']

shortname="rank"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('170','real')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   real(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Convert to real type', &
'', &
'SYNTAX', &
'   result = real(x [, kind])', &
'', &
'DESCRIPTION', &
'   real(x [, kind]) converts its argument X to a real type.', &
'', &
'ARGUMENTS', &
'   X        Shall be INTEGER, REAL, or COMPLEX.', &
'   KIND    (Optional) An INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   These functions return a REAL variable or array under', &
'   the following rules:', &
'', &
'   1. real(x) is converted to a default real type if X is an integer', &
'      or real variable.', &
'', &
'   2. real(x) is converted to a real type with the kind type parameter', &
'      of X if X is a complex variable.', &
'', &
'   3. real(x, kind) is converted to a real type with kind type', &
'      parameter KIND if X is a complex, integer, or real variable.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_real', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64', &
'      implicit none', &
'      complex              :: zr = (1.0, 2.0)', &
'      doubleprecision      :: xd=huge(3.0d0)', &
'      complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)', &
'', &
'      print *, real(zr), aimag(zr)', &
'      print *, dble(zd), aimag(zd)', &
'', &
'      write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)', &
'   end program demo_real', &
'', &
'  Results:', &
'', &
'   1.00000000       2.00000000', &
'   4.0000000000000000        5.0000000000000000', &
'   1.7976931348623157E+308   1.7976931348623157E+308   1.7976931348623157E+308', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   dble(3), float(3)', &
'']

shortname="real"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('171','repeat')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   repeat(3f) - [FORTRAN:INTRINSIC:CHARACTER] Repeated string concatenation', &
'', &
'SYNTAX', &
'   result = repeat(string, ncopies)', &
'', &
'DESCRIPTION', &
'   Concatenates NCOPIES copies of a string.', &
'', &
'ARGUMENTS', &
'   STRING    Shall be scalar and of type CHARACTER.', &
'   NCOPIES   Shall be scalar and of type INTEGER.', &
'', &
'RETURN VALUE', &
'   A new scalar of type CHARACTER built up from NCOPIES copies', &
'   of STRING.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_repeat', &
'    implicit none', &
'      write(*,*) repeat("x", 5)   ! "xxxxx"', &
'    end program demo_repeat', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="repeat"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('172','reshape')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   reshape(3f) - [FORTRAN:INTRINSIC:ARRAY RESHAPE] Function to reshape an array', &
'', &
'SYNTAX', &
'   result = reshape(source, shape[, pad, order])', &
'', &
'DESCRIPTION', &
'   Reshapes array SOURCE to correspond to SHAPE. If necessary,', &
'   the new array may be padded with elements from PAD or permuted', &
'   as defined by ORDER.', &
'', &
'ARGUMENTS', &
'   SOURCE    an array of any type.', &
'   SHAPE     an array of rank one and type INTEGER. Its', &
'             values must be positive or zero.', &
'   PAD       (Optional) an array of the same type as SOURCE.', &
'   ORDER     (Optional) an array of type INTEGER and the', &
'             same shape as SHAPE. Its values shall be a permutation of', &
'             the numbers from 1 to n, where n is the size of SHAPE. If', &
'             ORDER is absent, the natural ordering shall be assumed.', &
'', &
'RETURN VALUE', &
'   The result is an array of shape SHAPE with the same type as SOURCE.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_reshape', &
'    implicit none', &
'    integer :: i', &
'    integer, dimension(4) :: x=[(i,i=10,40,10)]', &
'      ! X is originally a vector with four elements', &
'      write(*,*) shape(x)                     ! prints "4"', &
'      write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"', &
'    end program demo_reshape', &
'', &
'  Results', &
'', &
'           4', &
'           2           2', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'SEE ALSO', &
'   shape(3)', &
'']

shortname="reshape"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('173','return')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   return(7f) - [FORTRAN:STATEMENT] completes execution of the instance', &
'   of the subprogram in which it appears', &
'', &
'SYNOPSIS', &
'   RETURN [scalar-int-expr]', &
'', &
'DESCRIPTION', &
'   Execution of the RETURN statement completes execution of the instance', &
'   of the subprogram in which it appears.', &
'', &
'   It is generally considered good practice to avoid having multiple', &
'   RETURN statements in a single subprogram. A RETURN is not required', &
'   in a subprogram as reaching the end of the subprogram is equivalent', &
'   to execution of a RETURN statement with no expression.', &
'', &
'   The RETURN statement must appear in the scoping unit of a function or', &
'   subroutine subprogram.', &
'', &
'OPTIONS', &
'   scalar-int-expr  Alternate returns are deprecated!', &
'                    If the expression appears and has a value n between', &
'                    1 and the number of asterisks in the dummy argument', &
'                    list, the CALL statement that invoked the subroutine', &
'                    transfers control to the statement identified by', &
'                    the nth alternate return specifier in the actual', &
'                    argument list of the referenced procedure. If the', &
'                    expression is omitted or has a value outside the', &
'                    required range, there is no transfer of control to', &
'                    an alternate return.', &
'', &
'                    The scalar-int-expr is allowed only in the scoping', &
'                    unit of a subroutine subprogram.', &
'EXAMPLE', &
'  Sample program', &
'', &
'   program demo_return', &
'      call tryreturn(1)', &
'      call tryreturn(10)', &
'   contains', &
'      subroutine tryreturn(i)', &
'         integer,intent(in) :: i', &
'         select case(i)', &
'          case(1)', &
'            write(*,*)''*one*''', &
'            return', &
'          case(2)', &
'            write(*,*)''*two*''', &
'            return', &
'          case default', &
'            write(*,*)''*default*''', &
'            return', &
'         end select', &
'         write(*,*)''*cannot get here*''', &
'         return', &
'      end subroutine tryreturn', &
'   end program demo_return', &
'', &
'  Results:', &
'', &
'    *one*', &
'    *default*', &
'', &
'  Sample program using alternate returns. Alternate returns are', &
'  an obsolescent feature.', &
'', &
'   program alt_return', &
'   implicit none', &
'      call one(2,*10,*20,*30)', &
'      write(*,*)''did not select alternate return''', &
'      goto 999', &
'   10 continue', &
'      write(*,*)''picked first alternate return''', &
'      goto 999', &
'   20 continue', &
'      write(*,*)''picked second alternate return''', &
'      goto 999', &
'   30 continue', &
'      write(*,*)''picked third alternate return''', &
'      goto 999', &
'   999 continue', &
'   end program alt_return', &
'   subroutine one(ipick,*,*,*)', &
'   implicit none', &
'   integer :: ipick', &
'      select case(ipick)', &
'       case(1)', &
'         write(*,*)''first alternate return selected''', &
'         return 1', &
'       case(2)', &
'         write(*,*)''second alternate return selected''', &
'         return 2', &
'       case(3)', &
'         write(*,*)''third alternate return selected''', &
'         return 3', &
'      end select', &
'      write(*,*)''no alternate return selected''', &
'   end subroutine one', &
'', &
'  Results:', &
'', &
'    second alternate return selected', &
'    picked second alternate return', &
' JSU', &
'']

shortname="return"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('174','rewind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   REWIND(7f) - [FORTRAN:FILE_POSITIONING] rewind specified sequential', &
'   access I/O unit', &
'', &
'SYNOPSIS', &
'   REWIND file-unit-number', &
'', &
'    REWIND ( [UNIT=]file-unit-number][,IOMSG=iomsg-variable] &', &
'    & [,IOSTAT=scalar-int-variable][,ERR=label] )', &
'', &
'DESCRIPTION', &
'   Execution of a REWIND statement causes the file connected to the', &
'   specified unit to be positioned at the beginning of the file (its', &
'   initial point).', &
'', &
'   If the file is already positioned at its initial point, execution of', &
'   this statement has no effect on the position of the file.', &
'', &
'   Execution of a REWIND statement for a file that is connected but does', &
'   not exist is permitted and has no effect on any file.', &
'', &
'OPTIONS', &
'   UNIT     unit number of file to rewind. A unit open for direct access', &
'            or stream access cannot be referenced by a REWIND (e.g. you', &
'            cannot typically rewind stdin and stdout). ', &
'   IOSTAT   (Optional) a compiler-specific number that indicates an', &
'            error occurred if non-zero. If not present and an error', &
'            occurs the program terminates.', &
'   IOMSG    (Optional) a message describing error IOSTAT if IOSTAT is', &
'            not zero.', &
'   ERR      (Optional) a label number to jump to if an error occurs', &
'', &
'EXAMPLE', &
'  An example of a REWIND statement is:', &
'', &
'   program demo_rewind', &
'   implicit none', &
'   character(len=256) :: line', &
'   character(len=256) :: mssge', &
'   integer            :: i', &
'   integer            :: ios', &
'      open(10,file=''demo_rewind.txt'') ! open a file', &
'      do i=1,100                      ! write lines to it', &
'         write(10,''(a,i0)'') ''line '',i', &
'      enddo', &
'      rewind(10, iostat=ios,iomsg=mssge)', &
'      if(ios.ne.0)then', &
'         write(*,*)''*error* '',trim(mssge)', &
'         stop', &
'      endif', &
'      write(*,*)''wrote 100 lines, but now at line ...''', &
'      read(10,''(a)'')line', &
'      write(*,''(a)'')line', &
'      read(10)', &
'      read(10)', &
'      read(10)', &
'      write(*,*)''skipped a few lines, now at ...''', &
'      read(10,''(a)'')line', &
'      write(*,''(a)'')line', &
'      close(10,status=''delete'')', &
'   end program demo_rewind', &
'', &
'SEE ALSO', &
'   The input/output statements are the OPEN, CLOSE, READ, WRITE, PRINT,', &
'   BACKSPACE, ENDFILE, REWIND, FLUSH, WAIT, and INQUIRE statements.', &
'', &
'   * The READ statement is a data transfer input statement.', &
'   * The WRITE statement and the PRINT statement are data transfer', &
'     output statements.', &
'   * The OPEN statement and the CLOSE statement are file connection', &
'     statements.', &
'   * The INQUIRE statement is a file inquiry statement.', &
'   * The BACKSPACE, ENDFILE, and REWIND statements are file positioning', &
'     statements.', &
'', &
'   JSU', &
'']

shortname="rewind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('175','rrspacing')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   rrspacing(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Reciprocal of', &
'   the relative spacing', &
'', &
'SYNTAX', &
'   result = rrspacing(x)', &
'', &
'DESCRIPTION', &
'   rrspacing(x) returns the reciprocal of the relative spacing of model', &
'   numbers near X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X.', &
'   The value returned is equal to', &
'   abs(fraction(x)) * float(radix(x))**digits(x).', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   spacing(3)', &
'']

shortname="rrspacing"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('176','same_type_as')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   same_type_as(3f) - [FORTRAN:INTRINSIC] Query dynamic types for equality', &
'', &
'SYNTAX', &
'   result = same_type_as(a, b)', &
'', &
'DESCRIPTION', &
'   Query dynamic types for equality.', &
'', &
'ARGUMENTS', &
'   A    Shall be an object of extensible declared type or unlimited polymorphic.', &
'   B    Shall be an object of extensible declared type or unlimited polymorphic.', &
'', &
'RETURN VALUE', &
'   The return value is a scalar of type default logical. It is true if and', &
'   only if the dynamic type of A is the same as the dynamic type of B.', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   extends_type_of(3)', &
'', &
'']

shortname="same_type_as"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('177','scale')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   scale(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Scale a real value', &
'', &
'SYNTAX', &
'   result = scale(x, i)', &
'', &
'DESCRIPTION', &
'   scale(x,i) returns x * radix(x)**i.', &
'', &
'ARGUMENTS', &
'   X    The type of the argument shall be a REAL.', &
'   I    The type of the argument shall be a INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X.', &
'   Its value is x * radix(x)**i.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_scale', &
'    implicit none', &
'      real :: x = 178.1387e-4', &
'      integer :: i = 5', &
'      print *, scale(x,i), x*radix(x)**i', &
'    end program demo_scale', &
'  Results:', &
'', &
'    0.570043862      0.570043862', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   radix(3)', &
'', &
'']

shortname="scale"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('178','scan')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   scan(3f) - [FORTRAN:INTRINSIC:CHARACTER] Scan a string for the presence', &
'   of a set of characters', &
'', &
'SYNTAX', &
'   result = scan(string, set[, back [, kind]])', &
'', &
'DESCRIPTION', &
'   Scans a STRING for any of the characters in a SET', &
'   of characters.', &
'', &
'   If BACK is either absent or equals FALSE, this function', &
'   returns the position of the leftmost character of STRING that is', &
'   in SET. If BACK equals TRUE, the rightmost position', &
'   is returned. If no character of SET is found in STRING, the', &
'   result is zero.', &
'', &
'ARGUMENTS', &
'   STRING  Shall be of type CHARACTER.', &
'   SET     Shall be of type CHARACTER.', &
'   BACK    (Optional) shall be of type LOGICAL.', &
'   KIND    (Optional) An INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_scan', &
'    implicit none', &
'      write(*,*) scan("fortran", "ao")          ! 2, found ''o''', &
'      write(*,*) scan("fortran", "ao", .true.)  ! 6, found ''a''', &
'      write(*,*) scan("fortran", "c++")         ! 0, found none', &
'    end program demo_scan', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="scan"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('179','selected_char_kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   selected_char_kind(3f) - [FORTRAN:INTRINSIC:KIND] Choose character', &
'   kind such as "Unicode"', &
'', &
'SYNTAX', &
'   result = selected_char_kind(name)', &
'', &
'DESCRIPTION', &
'   selected_char_kind(name) returns the kind value for the character', &
'   set named NAME, if a character set with such a name is supported, or', &
'   -1 otherwise. Currently, supported character sets include "ASCII"', &
'   and "DEFAULT" (iwhich are equivalent), and "ISO_10646" (Universal', &
'   Character Set, UCS-4) which is commonly known as "Unicode".', &
'', &
'ARGUMENTS', &
'   NAME    Shall be a scalar and of the default character type.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_selected_char_kind', &
'     use iso_fortran_env', &
'     implicit none', &
'     integer, parameter :: ascii = selected_char_kind ("ascii")', &
'     integer, parameter :: ucs4  = selected_char_kind (''ISO_10646'')', &
'', &
'     character(kind=ascii, len=26) :: alphabet', &
'     character(kind=ucs4,  len=30) :: hello_world', &
'', &
'     alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"', &
'     hello_world = ucs4_''Hello World and Ni Hao -- '' &', &
'                   // char (int (z''4F60''), ucs4)     &', &
'                   // char (int (z''597D''), ucs4)', &
'', &
'     write (*,*) alphabet', &
'', &
'     open (output_unit, encoding=''UTF-8'')', &
'     write (*,*) trim (hello_world)', &
'   end program demo_selected_char_kind', &
'', &
'STANDARD', &
'   [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="selected_char_kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('180','selected_int_kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   selected_int_kind(3f) - [FORTRAN:INTRINSIC:KIND] Choose integer kind', &
'', &
'SYNTAX', &
'   result = selected_int_kind(r)', &
'', &
'DESCRIPTION', &
'   selected_int_kind(r) return the kind value of the smallest integer', &
'   type that can represent all values ranging from -10**R (exclusive)', &
'   to 10**R (exclusive). If there is no integer kind that accommodates', &
'   this range, selected_int_kind returns -1.', &
'', &
'ARGUMENTS', &
'   R    Shall be a scalar and of type INTEGER.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_selected_int_kind', &
'   implicit none', &
'     integer,parameter :: k5 = selected_int_kind(5)', &
'     integer,parameter :: k15 = selected_int_kind(15)', &
'     integer(kind=k5) :: i5', &
'     integer(kind=k15) :: i15', &
'', &
'     print *, huge(i5), huge(i15)', &
'', &
'     ! the following inequalities are always true', &
'     print *, huge(i5) >= 10_k5**5-1', &
'     print *, huge(i15) >= 10_k15**15-1', &
'   end program demo_selected_int_kind', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="selected_int_kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('181','selected_real_kind')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   selected_real_kind(3f) - [FORTRAN:INTRINSIC:KIND] Choose real kind', &
'', &
'SYNTAX', &
'   result = selected_real_kind([p, r, radix])', &
'', &
'DESCRIPTION', &
'   selected_real_kind(p, r, radix) return the kind value of a real data', &
'   type with decimal precision of at least P digits, exponent range of', &
'   at least R, and with a radix of RADIX.', &
'', &
'ARGUMENTS', &
'   P      (Optional) shall be a scalar and of type INTEGER.', &
'   R      (Optional) shall be a scalar and of type INTEGER.', &
'   RADIX  (Optional) shall be a scalar and of type INTEGER.', &
'', &
'   Before [[Fortran 2008]], at least one of the arguments R or P', &
'   shall be present; since [[Fortran 2008]], they are assumed to be zero', &
'   if absent.', &
'', &
'RETURN VALUE', &
'   selected_real_kind returns the value of the kind type parameter of', &
'   a real data type with decimal precision of at least P digits, a', &
'   decimal exponent range of at least R, and with the requested', &
'   RADIX. If the RADIX parameter is absent, real kinds with', &
'   any radix can be returned. If more than one real data type meet the', &
'   criteria, the kind of the data type with the smallest decimal precision', &
'   is returned. If no real data type matches the criteria, the result is', &
'', &
'* -1 if the processor does not support a real data type with a', &
'  precision greater than or equal to P, but the R and RADIX', &
'  requirements can be fulfilled', &
'* -2 if the processor does not support a real type with an exponent', &
'  range greater than or equal to R, but P and RADIX are', &
'  fulfillable', &
'* -3 if RADIX but not P and R requirements are fulfillable', &
'* -4 if RADIX and either P or R requirements are fulfillable', &
'* -5 if there is no real type with the given RADIX', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_selected_real_kind', &
'   implicit none', &
'     integer,parameter :: p6 = selected_real_kind(6)', &
'     integer,parameter :: p10r100 = selected_real_kind(10,100)', &
'     integer,parameter :: r400 = selected_real_kind(r=400)', &
'     real(kind=p6) :: x', &
'     real(kind=p10r100) :: y', &
'     real(kind=r400) :: z', &
'', &
'     print *, precision(x), range(x)', &
'     print *, precision(y), range(y)', &
'     print *, precision(z), range(z)', &
'   end program demo_selected_real_kind', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later; with RADIX [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   precision(3), range(3), radix(3)', &
'']

shortname="selected_real_kind"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('182','set_exponent')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   set_exponent(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Set the', &
'   exponent of the model', &
'', &
'SYNTAX', &
'   result = set_exponent(x, i)', &
'', &
'DESCRIPTION', &
'   set_exponent(x, i) returns the real number whose fractional part', &
'   is that of X and whose exponent part is I.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'   I    Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X.', &
'   The real number whose fractional part', &
'   is that that of X and whose exponent part if I is returned;', &
'   it is fraction(x) * radix(x)**i.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_setexp', &
'    implicit none', &
'      real :: x = 178.1387e-4', &
'      integer :: i = 17', &
'      print *, set_exponent(x, i), fraction(x) * radix(x)**i', &
'    end program demo_setexp', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="set_exponent"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('183','shape')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   shape(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] Determine the shape of', &
'   an array', &
'', &
'SYNTAX', &
'   result = shape(source[, kind])', &
'', &
'DESCRIPTION', &
'   Determines the shape of an array.', &
'', &
'ARGUMENTS', &
'  SOURCE    Shall be an array or scalar of any type. If SOURCE is', &
'            a pointer it must be associated and allocatable arrays must', &
'            be allocated.', &
'  KIND      (Optional) An INTEGER initialization expression', &
'            indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   An INTEGER array of rank one with as many elements as SOURCE has', &
'   dimensions. The elements of the resulting array correspond to the', &
'   extend of SOURCE along the respective dimensions. If SOURCE is a', &
'   scalar, the result is the rank one array of size zero. If KIND is', &
'   absent, the return value has the default integer kind otherwise the', &
'   specified kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_shape', &
'   implicit none', &
'     integer, dimension(-1:1, -1:2) :: a', &
'     write(*,*) shape(a)             ! [ 3, 4 ]', &
'     write(*,*) size(shape(42))      ! [ ]', &
'   end program demo_shape', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later; with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   reshape(3), size(3)', &
'']

shortname="shape"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('184','shifta')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   shifta(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] shift bits right with fill', &
'', &
'SYNTAX', &
'   result = SHIFTA(I, SHIFT)', &
'', &
'DESCRIPTION', &
'   Returns a value corresponding to I with all of the bits shifted right', &
'   by SHIFT places. If the absolute value of SHIFT is greater than', &
'   BIT_SIZE(I), the value is undefined. Bits shifted out from the', &
'   right end are lost. The fill is arithmetic: the bits shifted in from', &
'   the left end are equal to the leftmost bit, which in two''s complement', &
'   representation is the sign bit.', &
'', &
'ARGUMENTS', &
'  I        The type shall be INTEGER.', &
'  SHIFT    The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   shiftl(3), shiftr(3)', &
'']

shortname="shifta"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('185','shiftl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   shiftl(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] shift bits left', &
'', &
'SYNTAX', &
'   result = SHIFTL(I, SHIFT)', &
'', &
'DESCRIPTION', &
'   Returns a value corresponding to I with all of the bits shifted left', &
'   by SHIFT places. If the absolute value of SHIFT is greater than', &
'   BIT_SIZE(I), the value is undefined. Bits shifted out from the left', &
'   end are lost, and bits shifted in from the right end are set to 0.', &
'', &
'ARGUMENTS', &
'  I       The type shall be INTEGER.', &
'  SHIFT   The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   shifta(3), shiftr(3)', &
'']

shortname="shiftl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('186','shiftr')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   shiftr(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] shift bits right', &
'', &
'SYNTAX', &
'   result = SHIFTR(I, SHIFT)', &
'', &
'DESCRIPTION', &
'   Returns a value corresponding to I with all of the bits shifted right', &
'   by SHIFT places. If the absolute value of SHIFT is greater than', &
'   BIT_SIZE(I), the value is undefined. Bits shifted out from the right', &
'   end are lost, and bits shifted in from the left end are set to 0.', &
'', &
'ARGUMENTS', &
'   I       The type shall be INTEGER.', &
'   SHIFT   The type shall be INTEGER.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of the same kind as I.', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   shifta(3), shiftl(3)', &
'']

shortname="shiftr"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('187','sign')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sign(3f) - [FORTRAN:INTRINSIC:NUMERIC] Sign copying function', &
'', &
'SYNTAX', &
'   result = sign(a, b)', &
'', &
'DESCRIPTION', &
'   sign(a,b) returns the value of A with the sign of B.', &
'', &
'ARGUMENTS', &
'   A    Shall be of type INTEGER or REAL', &
'   B    Shall be of the same type and kind as A', &
'', &
'RETURN VALUE', &
'   The kind of the return value is that of A and B.', &
'   If B >= 0 then the result is abs(a), else', &
'   it is -abs(a).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_sign', &
'    implicit none', &
'      print *, sign(-12,1)', &
'      print *, sign(-12,0)', &
'      print *, sign(-12,-1)', &
'', &
'      print *, sign(-12.,1.)', &
'      print *, sign(-12.,0.)', &
'      print *, sign(-12.,-1.)', &
'    end program demo_sign', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'']

shortname="sign"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('188','sin')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sin(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Sine function', &
'', &
'SYNTAX', &
'   result = sin(x)', &
'', &
'DESCRIPTION', &
'   sin(3f) computes the sine of an angle given the size of the angle', &
'   in radians.', &
'', &
'   The sine of an angle in a right-angled triangle is the ratio of the', &
'   length of the side opposite the given angle divided by the length of', &
'   the hypotenuse.', &
'', &
'ARGUMENTS', &
'   X       The type shall be REAL or COMPLEX in radians.', &
'', &
'RETURN VALUE', &
'   RESULT  The return value has the same type and kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program sample_sin', &
'    implicit none', &
'      real :: x = 0.0', &
'      x = sin(x)', &
'    end program sample_sin', &
'', &
'   HAVERSINE FORMULA', &
'', &
'   From the article on "Haversine formula" in Wikipedia:', &
'', &
'      The haversine formula is an equation important in navigation,', &
'      giving great-circle distances between two points on a sphere from', &
'      their longitudes and latitudes.', &
'', &
'   So to show the great-circle distance between the Nashville', &
'   International Airport (BNA) in TN, USA, and the Los Angeles', &
'   International Airport (LAX) in CA, USA you would start with their', &
'   latitude and longitude, commonly given as', &
'', &
'     BNA: N 36 degrees 7.2'',   W 86 degrees 40.2''', &
'     LAX: N 33 degrees 56.4'',  W 118 degrees 24.0''', &
'', &
'   which converted to floating-point values in degrees is:', &
'', &
'          Latitude Longitude', &
'     BNA   36.12,   -86.67', &
'     LAX   33.94,   -118.40', &
'', &
'  And then use the haversine formula to roughly calculate the distance', &
'  along the surface of the Earth between the locations:', &
'', &
'  Sample program:', &
'', &
'    program demo_sin', &
'    implicit none', &
'    real :: d', &
'        d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX', &
'        print ''(A,F9.4,A)'', ''distance: '',d,'' km''', &
'    contains', &
'    function haversine(latA,lonA,latB,lonB) result (dist)', &
'    !', &
'    ! calculate great circle distance in kilometers', &
'    ! given latitude and longitude in degrees', &
'    !', &
'    real,intent(in) :: latA,lonA,latB,lonB', &
'    real :: a,c,dist,delta_lat,delta_lon,lat1,lat2', &
'    real,parameter :: radius = 6371 ! mean earth radius in kilometers,', &
'    ! recommended by the International Union of Geodesy and Geophysics', &
'    real, parameter :: deg_to_rad = atan(1.0)/45.0 ! generate constant pi/180', &
'       delta_lat = deg_to_rad*(latB-latA)', &
'       delta_lon = deg_to_rad*(lonB-lonA)', &
'       lat1 = deg_to_rad*(latA)', &
'       lat2 = deg_to_rad*(latB)', &
'       a = (sin(delta_lat/2))**2 + cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2', &
'       c = 2*asin(sqrt(a))', &
'       dist = radius*c', &
'    end function haversine', &
'    end program demo_sin', &
'', &
'  Results:', &
'', &
'   distance: 2886.4446 km', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   asin(3), cos(3), tan(3)', &
'', &
' JSU', &
'']

shortname="sin"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('189','sinh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sinh(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Hyperbolic', &
'   sine function', &
'', &
'SYNTAX', &
'   result = sinh(x)', &
'', &
'DESCRIPTION', &
'   sinh(x) computes the hyperbolic sine of X.', &
'', &
'ARGUMENTS', &
'  X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_sinh', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128', &
'   implicit none', &
'     real(kind=real64) :: x = - 1.0_real64', &
'     x = sinh(x)', &
'   end program demo_sinh', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, for a complex argument [[Fortran 2008]] or later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   asinh(3)', &
'']

shortname="sinh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('190','size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   size(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] Determine the size of an array', &
'', &
'SYNTAX', &
'   result = size(array[, dim [, kind]])', &
'', &
'DESCRIPTION', &
'   Determine the extent of ARRAY along a specified dimension DIM,', &
'   or the total number of elements in ARRAY if DIM is absent.', &
'', &
'ARGUMENTS', &
'   ARRAY   Shall be an array of any type. If ARRAY is a pointer it', &
'           must be associated and allocatable arrays must be allocated.', &
'   DIM     (Optional) shall be a scalar of type INTEGER and its', &
'           value shall be in the range from 1 to n, where n equals the', &
'           rank of ARRAY.', &
'   KIND    (Optional) An INTEGER initialization expression', &
'           indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If', &
'   KIND is absent, the return value is of default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_size', &
'    implicit none', &
'    integer :: i, j', &
'    integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])', &
'       write(*,*) ''SIZE of simple one-dimensional array='', &', &
'       & size([ 11, 22, 33 ])    ! 3', &
'', &
'       write(*,*)''body''', &
'       write(*,*)''SHAPE(arr)       :'',shape(arr)', &
'       write(*,*)''SIZE(arr)        :'',size(arr)', &
'       write(*,*)''SIZE(arr,DIM=1)  :'',size(arr,dim=1)', &
'       write(*,*)''SIZE(arr,DIM=2)  :'',size(arr,dim=2)', &
'       write(*,*)''note lower bound is not "1"''', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'       write(*,*)''UBOUND(arr)      :'',ubound(arr)', &
'       write(*,*)''LBOUND(arr,DIM=1):'',lbound(arr,dim=1)', &
'       write(*,*)''UBOUND(arr,DIM=1):'',ubound(arr,dim=1)', &
'       write(*,*)''LBOUND(arr,DIM=2):'',lbound(arr,dim=2)', &
'       write(*,*)''UBOUND(arr,DIM=2):'',ubound(arr,dim=2)', &
'', &
'       call interfaced(arr,arr)', &
'       call nointerface(arr)', &
'    contains', &
'', &
'    subroutine interfaced(arr,arr2)', &
'    integer,intent(in)  :: arr(:,:)', &
'    integer,intent(in)  :: arr2(2,*)', &
'       !', &
'       write(*,*)''interfaced assumed-shape arr2ay''', &
'       !', &
'       !    source    argument of    shape    intrinsic at (1) must not be', &
'       ! an assumed size array', &
'       !!write(*,*)''SHAPE(arr2)       :'',shape(arr2)', &
'       ! The upper bound in the last dimension must appear in the reference', &
'       ! to the assumed size array    arr2    at (1)', &
'       !!write(*,*)''SIZE(arr2)        :'',size(arr2)', &
'       write(*,*)''SIZE(arr2,DIM=1)  :'',size(arr2,dim=1)', &
'       !    dim    argument of    size    intrinsic at (1) is not', &
'       !a valid dimension index', &
'       !!write(*,*)''SIZE(arr2,DIM=2)  :'',size(arr2,dim=2)', &
'       write(*,*)''note lower bound is "1"''', &
'       write(*,*)''LBOUND(arr2)      :'',lbound(arr2)', &
'       write(*,*)''LBOUND(arr2)      :'',lbound(arr2)', &
'       ! The upper bound in the last dimension must appear in the', &
'       ! reference to the assumed size array    arr2    at (1)', &
'       !!write(*,*)''UBOUND(arr2)      :'',ubound(arr2)', &
'       write(*,*)''LBOUND(arr2,DIM=1):'',lbound(arr2,dim=1)', &
'       write(*,*)''UBOUND(arr2,DIM=1):'',ubound(arr2,dim=1)', &
'       write(*,*)''LBOUND(arr2,DIM=2):'',lbound(arr2,dim=2)', &
'       !    dim    argument of    ubound    intrinsic at (1) is not', &
'       ! a valid dimension index', &
'       !!write(*,*)''UBOUND(arr2,DIM=2):'',ubound(arr2,dim=2)', &
'       !', &
'       write(*,*)''interfaced''', &
'       !', &
'       write(*,*)''SHAPE(arr)       :'',shape(arr)', &
'       write(*,*)''SIZE(arr)        :'',size(arr)', &
'       write(*,*)''SIZE(arr,DIM=1)  :'',size(arr,dim=1)', &
'       write(*,*)''SIZE(arr,DIM=2)  :'',size(arr,dim=2)', &
'       write(*,*)''note lower bound is "1"''', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'       write(*,*)''UBOUND(arr)      :'',ubound(arr)', &
'       write(*,*)''LBOUND(arr,DIM=1):'',lbound(arr,dim=1)', &
'       write(*,*)''UBOUND(arr,DIM=1):'',ubound(arr,dim=1)', &
'       write(*,*)''LBOUND(arr,DIM=2):'',lbound(arr,dim=2)', &
'       write(*,*)''UBOUND(arr,DIM=2):'',ubound(arr,dim=2)', &
'       !', &
'    end subroutine interfaced', &
'    !!', &
'    ! NOTE: If NOINTERFACE(3f) had an assumed-shape argument with :', &
'    !       for dimensions it could only be properly called with', &
'    !       an explicit interface', &
'    !!', &
'    subroutine nointerface(arr)', &
'    integer,intent(in) :: arr(3,*)', &
'       write(*,*)''nointerface''', &
'     ! SHAPE(3f) CANNOT BE USED ON AN ASSUMED SIZE ARRAY', &
'     !!write(*,*)''SHAPE(arr)       :'',shape(arr)', &
'     !!write(*,*)''SIZE(arr)        :'',size(arr)', &
'       write(*,*)''SIZE(arr,DIM=1)  :'',size(arr,dim=1)', &
'     ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION', &
'     !!write(*,*)''SIZE(arr,DIM=2)  :'',size(arr,dim=2)', &
'       write(*,*)''note lower bound is "1"''', &
'       write(*,*)''LBOUND(arr)      :'',lbound(arr)', &
'     !!write(*,*)''UBOUND(arr)      :'',ubound(arr)', &
'       write(*,*)''LBOUND(arr,DIM=1):'',lbound(arr,dim=1)', &
'       write(*,*)''UBOUND(arr,DIM=1):'',ubound(arr,dim=1)', &
'       write(*,*)''LBOUND(arr,DIM=2):'',lbound(arr,dim=2)', &
'     !!write(*,*)''UBOUND(arr,DIM=2):'',ubound(arr,dim=2)', &
'    end subroutine nointerface', &
'    !!', &
'    end program demo_size', &
'', &
'', &
'  Expected results:', &
'', &
'    SIZE of simple one-dimensional array=           3', &
'    body', &
'    SHAPE(arr)       :           3          11', &
'    SIZE(arr)        :          33', &
'    SIZE(arr,DIM=1)  :           3', &
'    SIZE(arr,DIM=2)  :          11', &
'    note lower bound is not "1"', &
'    LBOUND(arr)      :           0          -5', &
'    UBOUND(arr)      :           2           5', &
'    LBOUND(arr,DIM=1):           0', &
'    UBOUND(arr,DIM=1):           2', &
'    LBOUND(arr,DIM=2):          -5', &
'    UBOUND(arr,DIM=2):           5', &
'    interfaced assumed-shape arr2ay', &
'    SIZE(arr2,DIM=1)  :           2', &
'    note lower bound is "1"', &
'    LBOUND(arr2)      :           1           1', &
'    LBOUND(arr2)      :           1           1', &
'    LBOUND(arr2,DIM=1):           1', &
'    UBOUND(arr2,DIM=1):           2', &
'    LBOUND(arr2,DIM=2):           1', &
'    interfaced', &
'    SHAPE(arr)       :           3          11', &
'    SIZE(arr)        :          33', &
'    SIZE(arr,DIM=1)  :           3', &
'    SIZE(arr,DIM=2)  :          11', &
'    note lower bound is "1"', &
'    LBOUND(arr)      :           1           1', &
'    LBOUND(arr)      :           1           1', &
'    UBOUND(arr)      :           3          11', &
'    LBOUND(arr,DIM=1):           1', &
'    UBOUND(arr,DIM=1):           3', &
'    LBOUND(arr,DIM=2):           1', &
'    UBOUND(arr,DIM=2):          11', &
'    nointerface', &
'    SIZE(arr,DIM=1)  :           3', &
'    note lower bound is "1"', &
'    LBOUND(arr)      :           1           1', &
'    LBOUND(arr,DIM=1):           1', &
'    UBOUND(arr,DIM=1):           3', &
'    LBOUND(arr,DIM=2):           1', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   shape(3), reshape(3)', &
'']

shortname="size"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('191','sngl')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sngl(3f) - [FORTRAN:INTRINSIC:NUMERIC:TYPE] Convert double precision', &
'   real to default real', &
'', &
'SYNTAX', &
'   result = sngl(a)', &
'', &
'DESCRIPTION', &
'   sngl(a) converts the double precision real A to a default real', &
'   value. This is an archaic form of REAL that is specific to one type', &
'   for A.', &
'', &
'ARGUMENTS', &
'   A    The type shall be a double precision REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of type default REAL.', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   dble(3)', &
'']

shortname="sngl"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('192','spacing')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   spacing(3f) - [FORTRAN:INTRINSIC:MODEL_COMPONENTS] Smallest distance', &
'   between two numbers of a given type', &
'', &
'SYNTAX', &
'   result = spacing(x)', &
'', &
'DESCRIPTION', &
'   Determines the distance between the argument X and the nearest', &
'   adjacent number of the same type.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as the input argument X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_spacing', &
'    implicit none', &
'      integer, parameter :: sgl = selected_real_kind(p=6, r=37)', &
'      integer, parameter :: dbl = selected_real_kind(p=13, r=200)', &
'', &
'      write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686', &
'      write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686', &
'    end program demo_spacing', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   rrspacing(3)', &
'']

shortname="spacing"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('193','spread')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   spread(3f) - [FORTRAN:INTRINSIC:ARRAY CONSTRUCTION] Add a dimension', &
'   to an array', &
'', &
'SYNTAX', &
'   result = spread(source, dim, ncopies)', &
'', &
'DESCRIPTION', &
'   Replicates a SOURCE array NCOPIES times along a specified', &
'   dimension DIM.', &
'', &
'ARGUMENTS', &
'   SOURCE    Shall be a scalar or an array of any type and', &
'             a rank less than seven.', &
'   DIM       Shall be a scalar of type INTEGER with a', &
'             value in the range from 1 to n+1, where n equals the rank', &
'             of SOURCE.', &
'   NCOPIES   Shall be a scalar of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The result is an array of the same type as SOURCE and has rank n+1', &
'   where n equals the rank of SOURCE.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_spread', &
'    implicit none', &
'      integer :: a = 1, b(2) = [ 1, 2 ]', &
'      write(*,*) spread(a, 1, 2)            ! "1 1"', &
'      write(*,*) spread(b, 1, 2)            ! "1 1 2 2"', &
'    end program demo_spread', &
'', &
'     program example_spread', &
'     !  Author:', &
'     !    John Burkardt, 03 July 2006', &
'     implicit none', &
'     !', &
'     integer ( kind = 4 ) a1(4,3)', &
'     integer ( kind = 4 ) a2(3,4)', &
'     integer i', &
'     integer ( kind = 4 ) s', &
'     integer ( kind = 4 ) v(4)', &
'     !', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''TEST_SPREAD''', &
'     write ( *, ''(a)'' ) ''  SPREAD is a FORTRAN90 function which replicates''', &
'     write ( *, ''(a)'' ) ''  an array by adding a dimension.''', &
'     write ( *, ''(a)'' ) '' ''', &
'     !', &
'     s = 99', &
'     !', &
'     write ( *, ''(a,i6)'' ) ''  Suppose we have a scalar S = '', s', &
'     write ( *, ''(a)'' ) '' ''', &
'     !', &
'     v = spread ( s, 1, 4 )', &
'     !', &
'     write ( *, ''(a)'' ) ''  V = spread ( s, 1, 4 )''', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''  adds a new dimension (1) of extent 4''', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(4i6)'' ) v(1:4)', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''  Now first reset V to (1,2,3,4)''', &
'     v = [ 1, 2, 3, 4 ]', &
'     !', &
'     a1 = spread ( v, 2, 3 )', &
'     !', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''  A1 = spread ( v, 2, 3 )''', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''  adds a new dimension (2) of extent 3''', &
'     write ( *, ''(a)'' ) '' ''', &
'     do i = 1, 4', &
'       write ( *, ''(3i6)'' ) a1(i,1:3)', &
'     end do', &
'     !', &
'     a2 = spread ( v, 1, 3 )', &
'     !', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''  A2 = spread ( v, 1, 3 )''', &
'     write ( *, ''(a)'' ) '' ''', &
'     write ( *, ''(a)'' ) ''  adds a new dimension (1) of extent 3''', &
'     write ( *, ''(a)'' ) '' ''', &
'     do i = 1, 3', &
'       write ( *, ''(4i6)'' ) a2(i,1:4)', &
'     end do', &
'     end program example_spread', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   unpack(3)', &
'']

shortname="spread"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('194','sqrt')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sqrt(3f) - [FORTRAN:INTRINSIC:MATHEMATICS] Square-root function', &
'', &
'SYNTAX', &
'   result = sqrt(x)', &
'', &
'DESCRIPTION', &
'    sqrt(x) computes the square root of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value is of type REAL or COMPLEX. The kind type', &
'   parameter is the same as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_sqrt', &
'    use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'    & real32, real64, real128', &
'    implicit none', &
'      real(kind=real64) :: x = 2.0_real64', &
'      complex :: z = (1.0, 2.0)', &
'      x = sqrt(x)', &
'      z = sqrt(z)', &
'    end program demo_sqrt', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'']

shortname="sqrt"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('195','stop')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   STOP(7f),ALLSTOP(7f)  - [FORTRAN:STATEMENT] initiates termination of', &
'   execution', &
'', &
'SYNOPSIS', &
'  STOP [ stop-code ]', &
'  ERROR STOP [ stop-code ]', &
'', &
'  stop-code    is  scalar-char-initialization-expr', &
'               or  scalar-int-initialization-expr', &
'', &
'  The scalar-char-initialization-expr shall be of default kind.', &
'', &
'  The scalar-int-initialization-expr shall be of default kind.', &
'', &
'DESCRIPTION', &
'  A STOP statement will cause the program to terminate normally. It may', &
'  provide additional information in the form of output or a system status', &
'  code, depending on the system.', &
'', &
'  Any messages generated appear on the ERROR_UNIT file, as identified in', &
'  the intrinsic module ISO_FORTRAN_ENV. This unit is often referred to as', &
'  "stderr". It is recommended that systems write the value of the stop', &
'  code whether numeric or a string.', &
'', &
'  Note that although STOP is a "normal" termination system status codes', &
'  or "exit codes" are often used for error processing in many scripting', &
'  languages. This code may be detectable by EXECUTE_SYSTEM_COMMAND(3f).', &
'', &
'  Execution of an ERROR STOP statement initiates error termination of', &
'  an execution, which on several systems includes the output from', &
'  a traceback.', &
'', &
'  So when an image is terminated by a STOP or ERROR STOP statement, its', &
'  stop code, if any, is made available in a processor-dependent manner.', &
'', &
'  If any exception is signaling on a stopped image, the processor issues a', &
'  warning indicating which exceptions are signaling;', &
'', &
'  When normal termination occurs on more than one image, it is expected', &
'  that a processor-dependent summary of any stop codes and signaling', &
'  exceptions will be made available.', &
'', &
'  If an integer stop-code is used as the process exit status,', &
'  the processor might be able to interpret only values within a limited', &
'  range, or only a limited portion of the integer value (for example,', &
'  only the least-significant 8 bits).', &
'', &
'  If the stop-code is of type character or does not appear, or if an', &
'  END PROGRAM statement is executed, it is recommended that the value', &
'  zero be supplied as the process exit status, if the processor supports', &
'  that concept.', &
'', &
'EXAMPLE', &
'  Sample:', &
'', &
'   program demo_stop', &
'   use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT', &
'   implicit none', &
'   integer :: stopcode', &
'   ! Normal terminations', &
'      ! A STOP with no parameter is a normal termination and generally', &
'      ! returns a zero status value if the system supports return statuses', &
'      stop', &
'      ! All other stops are error stops', &
'      stop 10', &
'      stop ''That is all, folks!''', &
'      stopcode=11', &
'      stop stopcode', &
'   ! Error terminations', &
'      ! ERROR STOP is always an error stop, even without a stop-code', &
'      error stop', &
'      ! ERROR STOP often displays a traceback but that is not required', &
'      error stop 10', &
'      error stop ''That is all, folks!''', &
'      error stop stopcode', &
'   end program demo_stop', &
'', &
' JSU', &
'']

shortname="stop"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('196','storage_size')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   storage_size(3f) - [FORTRAN:INTRINSIC:BIT INQUIRY] Storage size in bits', &
'', &
'SYNTAX', &
'   result = storage_size(a [, kind])', &
'', &
'DESCRIPTION', &
'   Returns the storage size of argument A in bits.', &
'', &
'ARGUMENTS', &
'   A      Shall be a scalar or array of any type.', &
'   KIND   (Optional) shall be a scalar integer constant expression.', &
'', &
'RETURN VALUE', &
'   The result is a scalar integer with the kind type parameter specified', &
'   by KIND (or default integer type if KIND is missing). The result', &
'   value is the size expressed in bits for an element of an array that', &
'   has the dynamic type and type parameters of A.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'    program demo_storage_size', &
'    implicit none', &
'       write(*,*)''size of integer '',storage_size(0)', &
'       write(*,*)''size of real    '',storage_size(0.0)', &
'       write(*,*)''size of logical '',storage_size(.true.)', &
'       write(*,*)''size of complex '',storage_size((0.0,0.0))', &
'       write(*,*)''size of integer array '',storage_size([0,1,2,3,4,5,6,7,8,9])', &
'    end program demo_storage_size', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   Inquiry function', &
'', &
'SEE ALSO', &
'   c_sizeof(3)', &
'']

shortname="storage_size"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('197','sum')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sum(3f) - [FORTRAN:INTRINSIC:ARRAY REDUCTION] sum the elements of an array', &
'', &
'SYNTAX', &
' Calling sequence:', &
'', &
'   result = sum(array[, mask])', &
'   result = sum(array, dim[, mask])', &
'', &
'DESCRIPTION', &
'   Adds the elements of ARRAY along dimension DIM if the corresponding', &
'   element in MASK is TRUE.', &
'', &
'ARGUMENTS', &
'   array    Shall be an array of type INTEGER, REAL or COMPLEX.', &
'   dim      (Optional) shall be a scalar of type INTEGER with a value', &
'            in the range from 1 to n, where n equals the rank of ARRAY.', &
'   mask     (Optional) shall be of type LOGICAL and either be a', &
'            scalar or an array of the same shape as ARRAY.', &
'', &
'RETURN VALUE', &
'   The result is of the same type as ARRAY.', &
'', &
'   If dim(3f) is absent, a scalar with the sum of all elements in ARRAY', &
'   is returned. Otherwise, an array of rank n-1, where n equals the', &
'   rank of ARRAY, and a shape similar to that of ARRAY with dimension', &
'   DIM dropped is returned.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program simple_sum', &
'    implicit none', &
'      integer :: x(5) = [ 1, 2, 3, 4 ,5 ]', &
'      print *, sum(x)                        ! all elements, sum = 15', &
'      print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9', &
'    end program simple_sum', &
'', &
'  Demonstrate Fortran 90 SUM function with MASK option', &
'', &
'   program demo_sum', &
'   ! John Mahaffy  2/16/96', &
'   implicit none', &
'   integer nd, ndh, nduh, j', &
'   parameter (nd=10,ndh=nd/2, nduh=nd-ndh)', &
'   real csum, cpsum, cbpsum', &
'   real, dimension(nd):: c=[(j, j=-1,nd-2)], b', &
'   data b/ndh*-1.0, nduh*2.0/', &
'      csum= sum(c(1:nd))', &
'      cpsum= sum (c(1:nd), mask=c.gt.0)', &
'      cbpsum= sum(c(1:nd), mask=b.gt.0.0)', &
'      print *, ''Sum of all elements in c = '' , csum', &
'      print *, ''Sum of Positive elements in c = '', cpsum', &
'      print *, ''Sum of elements in c when corresponding elements in b>0'', &', &
'      & '' ='', cbpsum', &
'   end program demo_sum', &
'', &
'  Results:', &
'', &
'    Sum of all elements in c =    35.0000000', &
'    Sum of Positive elements in c =    36.0000000', &
'    Sum of elements in c when corresponding elements in b>0 =   30.0000000', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   intrinsics', &
'']

shortname="sum"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('198','system_clock')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   system_clock(3f) - [FORTRAN:INTRINSIC:SYSTEM ENVIRONMENT] Return', &
'   numeric data from a real-time clock.', &
'', &
'SYNTAX', &
'  subroutine system_clock([count, count_rate, count_max])', &
'', &
'   integer,intent(out),optional  :: count', &
'   integer,intent(out),optional  :: count_rate', &
'      or', &
'   real,intent(out),optional     :: count_rate', &
'   integer,intent(out,optional   :: count_max', &
'', &
'DESCRIPTION', &
'   system_clock lets you measure durations of time with the precision', &
'   of the smallest time increment generally available on a system by', &
'   returning processor-dependent values based on the current value of the', &
'   processor clock. The CLOCK value is incremented by one for each clock', &
'   count until the value count_max is reached and is then reset to zero', &
'   at the next count. CLOCK therefore is a modulo value that lies in the', &
'   range 0 to count_max. count_rate and count_max are assumed constant', &
'   (even though CPU rates can vary on a single platform).', &
'', &
'   count_rate is system dependent and can vary depending on the kind of', &
'   the arguments.', &
'', &
'   If there is no clock, or querying the clock fails, COUNT is set to', &
'   -huge(count), and count_rate and count_max are set to zero.', &
'', &
'   system_clock is typically used to measure short time intervals (system', &
'   clocks may be 24-hour clocks or measure processor clock ticks since', &
'   boot, for example). It is most often used for measuring or tracking', &
'   the time spent in code blocks in lieu of using profiling tools.', &
'', &
'ARGUMENTS', &
'    COUNT        (optional) shall be an integer scalar.', &
'                 It is assigned a processor-dependent value based on the', &
'                 current value of the processor clock, or -HUGE (COUNT)', &
'                 if there is no clock. The processor-dependent value is', &
'                 incremented by one for each clock count until the value', &
'                 COUNT_MAX is reached and is reset to zero at the next', &
'                 count. It lies in the range 0 to COUNT_MAX if there is', &
'                 a clock.', &
'    COUNT_RATE   (optional) shall be an integer or real scalar.', &
'                 It is assigned a processor-dependent approximation', &
'                 to the number of processor clock counts per second,', &
'                 or zero if there is no clock.', &
'    COUNT_MAX    (optional) shall be an integer scalar. It is assigned the', &
'                 maximum value that COUNT can have, or zero if there is', &
'                 no clock.', &
'', &
'EXAMPLE', &
'   Sample program:', &
'', &
'      program demo_system_clock', &
'      implicit none', &
'        integer :: count, count_rate, count_max', &
'        call system_clock(count, count_rate, count_max)', &
'        write(*,*) count, count_rate, count_max', &
'      end program demo_system_clock', &
'', &
'   If the processor clock is a 24-hour clock that registers time at', &
'   approximately 18.20648193 ticks per second, at 11:30 A.M. the reference', &
'', &
'      CALL SYSTEM_CLOCK (COUNT = C, COUNT_RATE = R, COUNT_MAX = M)', &
'', &
'   defines', &
'', &
'      C = (11*3600+30*60)*18.20648193 = 753748,', &
'      R = 18.20648193, and', &
'      M = 24*3600*18.20648193-1 = 1573039.', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Subroutine', &
'', &
'SEE ALSO', &
'   date_and_time(3), cpu_time(3)', &
'']

shortname="system_clock"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('199','tan')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   tan(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Tangent function', &
'', &
'SYNTAX', &
'   result = tan(x)', &
'', &
'DESCRIPTION', &
'   tan(x) computes the tangent of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has the same type and kind as X.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_tan', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'   & real32, real64, real128', &
'   implicit none', &
'   real(kind=real64) :: x = 0.165_real64', &
'     x = tan(x)', &
'   end program demo_tan', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later.', &
'   For a complex argument, [[Fortran 2008]] or later.', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   atan(3), cos(3), sin(3)', &
'']

shortname="tan"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('200','tanh')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   tanh(3f) - [FORTRAN:INTRINSIC:MATHEMATICS:TRIGONOMETRIC] Hyperbolic', &
'   tangent function', &
'', &
'SYNTAX', &
'    x = tanh(x)', &
'', &
'DESCRIPTION', &
'   tanh(X) computes the hyperbolic tangent of X.', &
'', &
'ARGUMENTS', &
'   X    The type shall be REAL or COMPLEX.', &
'', &
'RETURN VALUE', &
'   The return value has same type and kind as X. If X is', &
'   complex, the imaginary part of the result is in radians. If X', &
'   is REAL, the return value lies in the range', &
'', &
'      -1 <= tanh(x) <= 1.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_tanh', &
'   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128', &
'   implicit none', &
'     real(kind=real64) :: x = 2.1_real64', &
'     x = tanh(x)', &
'   end program demo_tanh', &
'', &
'STANDARD', &
'   [[FORTRAN 77]] and later, for a complex argument [[Fortran 2008]] or later', &
'', &
'CLASS', &
'   [[Elemental procedure|Elemental function]]', &
'', &
'SEE ALSO', &
'   atanh(3)', &
'']

shortname="tanh"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('201','this_image')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   this_image(3f) - [FORTRAN:INTRINSIC:COLLECTIVE] Cosubscript index of', &
'   this image', &
'', &
'SYNTAX', &
'   result = this_image()', &
'   result = this_image(distance)', &
'   result = this_image(coarray [, dim])', &
'', &
'DESCRIPTION', &
'   Returns the cosubscript for this image.', &
'', &
'ARGUMENTS', &
'  DISTANCE  (optional, intent(in)) Nonnegative scalar integer', &
'            (not permitted together with COARRAY).', &
'', &
'  COARRAY   Coarray of any type (optional; if DIM present,', &
'            required).', &
'', &
'  DIM       default integer scalar (optional). If present, DIM shall', &
'            be between one and the corank of COARRAY.', &
'', &
'RETURN VALUE', &
'   Default integer. If COARRAY is not present, it is scalar; if DISTANCE', &
'   is not present or has value 0, its value is the image index on the', &
'   invoking image for the current team, for values smaller or equal', &
'   distance to the initial team, it returns the image index on the', &
'   ancestor team which has a distance of DISTANCE from the invoking', &
'   team. If DISTANCE is larger than the distance to the initial team,', &
'   the image index of the initial team is returned. Otherwise when', &
'   the COARRAY is present, if DIM is not present, a rank-1 array with', &
'   corank elements is returned, containing the cosubscripts for COARRAY', &
'   specifying the invoking image. If DIM is present, a scalar is returned,', &
'   with the value of the DIM element of this_image(coarray).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'   program demo_this_image', &
'   implicit none', &
'   integer :: value[*]', &
'   integer :: i', &
'      value = this_image()', &
'      sync all', &
'      if (this_image() == 1) then', &
'        do i = 1, num_images()', &
'          write(*,''(2(a,i0))'') ''value['', i, ''] is '', value[i]', &
'        end do', &
'      endif', &
'   end program demo_this_image', &
'   !', &
'   ! Check whether the current image is the initial image', &
'   if (this_image(huge(1)) /= this_image())', &
'   error stop "something is rotten here"', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later. With DISTANCE argument, [[TS 18508]] or later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   num_images(3), image_index(3)', &
'']

shortname="this_image"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('202','tiny')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   tiny(3f) - [FORTRAN:INTRINSIC:NUMERIC MODEL] Smallest positive number', &
'   of a real kind', &
'', &
'SYNTAX', &
'   result = tiny(x)', &
'', &
'DESCRIPTION', &
'   tiny(x) returns the smallest positive (non zero) number', &
'   in the model of the type of X.', &
'', &
'ARGUMENTS', &
'   X    Shall be of type REAL.', &
'', &
'RETURN VALUE', &
'   The return value is of the same type and kind as X', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_huge_tiny', &
'    implicit none', &
'      print *, huge(0), huge(0.0), huge(0.0d0)', &
'      print *, tiny(0.0), tiny(0.0d0)', &
'    end program demo_huge_tiny', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'']

shortname="tiny"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('203','trailz')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   trailz(3f) - [FORTRAN:INTRINSIC:BIT INQUIRY] Number of trailing zero', &
'   bits of an integer', &
'', &
'SYNTAX', &
'   result = trailz(i)', &
'     integer :: result', &
'     integer(kind=NNN),intent(in) :: i', &
'', &
'DESCRIPTION', &
'  TRAILZ returns the number of trailing zero bits of an INTEGER value', &
'', &
'ARGUMENTS', &
'   I    Shall be of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The type of the return value is the default INTEGER.', &
'   If all the bits of I are zero, the result value is bit_size(I).', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_trailz', &
'    use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'    & int8, int16, int32, int64', &
'    implicit none', &
'    integer(kind=int64) :: i, value', &
'      write(*,*)''Default integer:''', &
'      write(*,*)''bit_size='',bit_size(0)', &
'      write(*,''(1x,i3,1x,i3,1x,b0)'')-1,trailz(1),-1', &
'      write(*,''(1x,i3,1x,i3,1x,b0)'')0,trailz(0),0', &
'      write(*,''(1x,i3,1x,i3,1x,b0)'')1,trailz(1),1', &
'      write(*,''(" huge(0)=",i0,1x,i0,1x,b0)'') &', &
'      & huge(0),trailz(huge(0)),huge(0)', &
'      write(*,*)', &
'      write(*,*)''integer(kind=int64):''', &
'', &
'      do i=-1,62,5', &
'         value=2**i', &
'         write(*,''(1x,i19,1x,i3)'')value,trailz(value)', &
'      enddo', &
'      value=huge(i)', &
'      write(*,''(1x,i19,1x,i3,"(huge(0_int64))")'')value,trailz(value)', &
'', &
'      do i=-1,62,5', &
'         value=2**i', &
'         write(*,''(1x,i3,2x,b64.64)'')i,value', &
'      enddo', &
'      value=huge(i)', &
'      write(*,''(1x,a,1x,b64.64)'') "huge",value', &
'', &
'    end program demo_trailz', &
'', &
'  Results:', &
'', &
'    Default integer:', &
'    bit_size=          32', &
'     -1   0 11111111111111111111111111111111', &
'      0  32 0', &
'      1   0 1', &
'    huge(0)=2147483647 0 1111111111111111111111111111111', &
'   ', &
'    integer(kind=int64):', &
'                      0  64', &
'                     16   4', &
'                    512   9', &
'                  16384  14', &
'                 524288  19', &
'               16777216  24', &
'              536870912  29', &
'            17179869184  34', &
'           549755813888  39', &
'         17592186044416  44', &
'        562949953421312  49', &
'      18014398509481984  54', &
'     576460752303423488  59', &
'    9223372036854775807   0(huge(0_int64))', &
'     -1  0000000000000000000000000000000000000000000000000000000000000000', &
'      4  0000000000000000000000000000000000000000000000000000000000010000', &
'      9  0000000000000000000000000000000000000000000000000000001000000000', &
'     14  0000000000000000000000000000000000000000000000000100000000000000', &
'     19  0000000000000000000000000000000000000000000010000000000000000000', &
'     24  0000000000000000000000000000000000000001000000000000000000000000', &
'     29  0000000000000000000000000000000000100000000000000000000000000000', &
'     34  0000000000000000000000000000010000000000000000000000000000000000', &
'     39  0000000000000000000000001000000000000000000000000000000000000000', &
'     44  0000000000000000000100000000000000000000000000000000000000000000', &
'     49  0000000000000010000000000000000000000000000000000000000000000000', &
'     54  0000000001000000000000000000000000000000000000000000000000000000', &
'     59  0000100000000000000000000000000000000000000000000000000000000000', &
'    huge 0111111111111111111111111111111111111111111111111111111111111111', &
'', &
'STANDARD', &
'   [[Fortran 2008]] and later', &
'', &
'CLASS', &
'   [[Elemental function]]', &
'', &
'SEE ALSO', &
'   bit_size(3), popcnt(3), poppar(3), leadz(3)', &
' JSU', &
'']

shortname="trailz"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('204','transfer')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   transfer(3f) - [FORTRAN:INTRINSIC:BIT MANIPULATION] Transfer bit patterns', &
'', &
'SYNTAX', &
'   result = transfer(source, mold[, size])', &
'', &
'DESCRIPTION', &
'   Interprets the bitwise representation of SOURCE in memory as if it', &
'   is the representation of a variable or array of the same type and', &
'   type parameters as MOLD.', &
'', &
'   This is approximately equivalent to the C concept of *casting* one', &
'   type to another.', &
'', &
'ARGUMENTS', &
'   SOURCE   Shall be a scalar or an array of any type.', &
'   MOLD     Shall be a scalar or an array of any type.', &
'   SIZE     (Optional) shall be a scalar of type INTEGER.', &
'', &
'RETURN VALUE', &
'   The result has the same type as MOLD, with the bit level representation', &
'   of SOURCE. If SIZE is present, the result is a one-dimensional array', &
'   of length SIZE. If SIZE is absent but MOLD is an array (of any size or', &
'   shape), the result is a one-dimensional array of the minimum length', &
'   needed to contain the entirety of the bitwise representation of', &
'   SOURCE. If SIZE is absent and MOLD is a scalar, the result is a scalar.', &
'', &
'   If the bitwise representation of the result is longer than that of', &
'   SOURCE, then the leading bits of the result correspond to those of', &
'   SOURCE and any trailing bits are filled arbitrarily.', &
'', &
'   When the resulting bit representation does not correspond to a valid', &
'   representation of a variable of the same type as MOLD, the results are', &
'   undefined, and subsequent operations on the result cannot be guaranteed', &
'   to produce sensible behavior. For example, it is possible to create', &
'   LOGICAL variables for which VAR and .not. var both appear to be true.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_transfer', &
'    implicit none', &
'      integer :: x = 2143289344', &
'      print *, transfer(x, 1.0)    ! prints "nan" on i686', &
'    end program demo_transfer', &
'', &
'COMMENTS', &
'   [[Joe Krahn]]: Fortran uses **molding** rather than **casting**.', &
'', &
'   Casting, as in C, is an in-place reinterpretation. A cast is a device', &
'   that is built around an object to change its shape.', &
'', &
'   Fortran TRANSFER reinterprets data out-of-place. It can be considered', &
'   **molding** rather than casting. A **mold** is a device that confers', &
'   a shape onto an object placed into it.', &
'', &
'   The advantage of molding is that data is always valid in the context', &
'   of the variable that holds it. For many cases, a decent compiler', &
'   should optimize TRANSFER into a simple assignment.', &
'', &
'   There are disadvantages of this approach. It is problematic to', &
'   define a union of data types because you must know the largest data', &
'   object, which can vary by compiler or compile options. In many cases,', &
'   an EQUIVALENCE would be far more effective, but Fortran Standards', &
'   committees seem oblivious to the benefits of EQUIVALENCEs when used', &
'   sparingly.', &
'', &
'STANDARD', &
'   [[Fortran 90]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="transfer"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('205','transpose')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   transpose(3f) - [FORTRAN:INTRINSIC:ARRAY MANIPULATION] Transpose an', &
'   array of rank two', &
'', &
'SYNTAX', &
'   result = transpose(matrix)', &
'', &
'DESCRIPTION', &
'   Transpose an array of rank two. Element (i, j) of the result has the', &
'   value matrix(j, i), for all i, j.', &
'', &
'ARGUMENTS', &
'   MATRIX    Shall be an array of any type and have a rank of two.', &
'', &
'RETURN VALUE', &
'   The result has the same type as MATRIX, and has shape', &
'   [ m, n ] if MATRIX has shape [ n, m ].', &
'', &
'EXAMPLE', &
'   Sample program:', &
'', &
'    program demo_transpose', &
'    implicit none', &
'    integer,save :: xx(3,5)= reshape([&', &
'        1,  2,  3,  4,  5,    &', &
'       10, 20, 30, 40, 50,    &', &
'       11, 22, 33, 44, -1055  &', &
'     ],shape(xx),order=[2,1])', &
'    call print_matrix_int(''xx array:'',xx)', &
'    call print_matrix_int(''xx array transposed:'',transpose(xx))', &
'    contains', &
'    subroutine print_matrix_int(title,arr)', &
'    ! print small 2d integer arrays in row-column format', &
'    implicit none', &
'    character(len=*),intent(in)  :: title', &
'    integer,intent(in)           :: arr(:,:)', &
'    integer                      :: i', &
'    character(len=:),allocatable :: biggest', &
'       write(*,*)trim(title)  ! print title', &
'       biggest=''           ''  ! make buffer to write integer into', &
'       ! find how many characters to use for integers', &
'       write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'       ! use this format to write a row', &
'       biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'       ! print one row of array at a time', &
'       do i=1,size(arr,dim=1)', &
'          write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'          write(*,''(" ]")'')', &
'       enddo', &
'    end subroutine print_matrix_int', &
'    end program demo_transpose', &
'', &
'   Results:', &
'', &
'    xx array:', &
'    > [     1,     2,     3,     4,     5 ]', &
'    > [    10,    20,    30,    40,    50 ]', &
'    > [    11,    22,    33,    44, -1055 ]', &
'    xx array transposed:', &
'    > [     1,    10,    11 ]', &
'    > [     2,    20,    22 ]', &
'    > [     3,    30,    33 ]', &
'    > [     4,    40,    44 ]', &
'    > [     5,    50, -1055 ]', &
'', &
'STANDARD', &
'   Fortran 95 and later', &
'', &
'CLASS', &
'   Transformational function', &
'']

shortname="transpose"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('206','trim')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   trim(3f) - [FORTRAN:INTRINSIC:CHARACTER] Remove trailing blank', &
'   characters of a string', &
'', &
'SYNTAX', &
'   result = trim(string)', &
'', &
'DESCRIPTION', &
'   Removes trailing blank characters of a string.', &
'', &
'ARGUMENTS', &
'   STRING    Shall be a scalar of type CHARACTER.', &
'', &
'RETURN VALUE', &
'   A scalar of type CHARACTER which length is that of STRING', &
'   less the number of trailing blanks.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_trim', &
'    implicit none', &
'      character(len=10), parameter :: s = "gfortran  "', &
'      write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks', &
'    end program demo_trim', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="trim"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('207','ubound')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   ubound(3f) - [FORTRAN:INTRINSIC:ARRAY INQUIRY] Upper dimension bounds', &
'   of an array', &
'', &
'SYNTAX', &
'   result = ubound(array [, dim [, kind]])', &
'', &
'DESCRIPTION', &
'   Returns the upper bounds of an array, or a single upper bound', &
'   along the DIM dimension.', &
'', &
'ARGUMENTS', &
'   ARRAY    Shall be an array, of any type.', &
'   DIM      (Optional) Shall be a scalar INTEGER.', &
'   KIND     (Optional) An INTEGER initialization expression', &
'            indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If KIND is', &
'   absent, the return value is of default integer kind. If DIM is absent,', &
'   the result is an array of the upper bounds of ARRAY. If DIM is present,', &
'   the result is a scalar corresponding to the upper bound of the array', &
'   along that dimension. If ARRAY is an expression rather than a whole', &
'   array or array structure component, or if it has a zero extent along', &
'   the relevant dimension, the upper bound is taken to be the number of', &
'   elements along the relevant dimension.', &
'', &
'EXAMPLE', &
'   Note that in my opinion this function should not be used on', &
'   assumed-size arrays or in any function without an explicit', &
'   interface. Errors can occur if there is no interface defined.', &
'', &
' Sample program', &
'', &
'  ! program demo_ubound', &
'  module m2_bounds', &
'  implicit none', &
'   contains', &
'      subroutine msub(arr)', &
'         !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array', &
'         integer,intent(in) :: arr(:)', &
'         write(*,*)''MSUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'      end subroutine msub', &
'   end module m2_bounds', &
'', &
'   use m2_bounds, only : msub', &
'   implicit none', &
'   interface', &
'      subroutine esub(arr)', &
'      integer,intent(in) :: arr(:)', &
'      end subroutine esub', &
'   end interface', &
'   integer :: arr(-10:10)', &
'      write(*,*)''MAIN: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'      & ''SIZE='',size(arr)', &
'      call csub()', &
'      call msub(arr)', &
'      call esub(arr)', &
'   contains', &
'      subroutine csub', &
'         write(*,*)''CSUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'      end subroutine csub', &
'   end', &
'', &
'   subroutine esub(arr)', &
'   implicit none', &
'   integer,intent(in) :: arr(:)', &
'      ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE', &
'      ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)', &
'      write(*,*)''ESUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'      & ''SIZE='',size(arr)', &
'   end subroutine esub', &
'  !end program demo_ubound', &
'', &
' Expected output', &
'', &
'  MAIN: LOWER=         -10 UPPER=          10 SIZE=          21', &
'  CSUB: LOWER=         -10 UPPER=          10 SIZE=          21', &
'  MSUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'  ESUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   [[Inquiry function]]', &
'', &
'SEE ALSO', &
'   lbound(3), co_ubound(3), co_lbound(3)', &
'']

shortname="ubound"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('208','unpack')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   unpack(3f) - [FORTRAN:INTRINSIC:ARRAY CONSTRUCTION] Store the elements', &
'   of a vector in an array of higher rank', &
'', &
'SYNTAX', &
'   result = unpack(vector, mask, field)', &
'', &
'DESCRIPTION', &
'   Store the elements of VECTOR in an array of higher rank.', &
'', &
'ARGUMENTS', &
'   VECTOR   Shall be an array of any type and rank one. It shall', &
'            have at least as many elements as MASK has TRUE values.', &
'   MASK     Shall be an array of type LOGICAL.', &
'   FIELD    Shall be of the same type as VECTOR and have the same', &
'            shape as MASK.', &
'', &
'RETURN VALUE', &
'   The resulting array corresponds to FIELD with TRUE elements', &
'   of MASK replaced by values from VECTOR in array element order.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_unpack', &
'    implicit none', &
'      integer :: vector(2)  = [1,1]', &
'      logical :: mask(4)  = [ .true., .false., .false., .true. ]', &
'      integer :: field(2,2) = 0, unity(2,2)', &
'', &
'      ! result: unity matrix', &
'      unity = unpack(vector, reshape(mask, [2,2]), field)', &
'    end program demo_unpack', &
'', &
'STANDARD', &
'   Fortran 95 and later', &
'', &
'CLASS', &
'   Transformational function', &
'', &
'SEE ALSO', &
'   pack(3), spread(3)', &
'']

shortname="unpack"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif


case('209','verify')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   verify(3f) - [FORTRAN:INTRINSIC:CHARACTER] Scan a string for the', &
'   absence of a set of characters', &
'', &
'SYNTAX', &
'   result = verify(string, set[, back [, kind]])', &
'', &
'DESCRIPTION', &
'   Verifies that all the characters in STRING belong to the set of', &
'   characters in SET.', &
'', &
'   If BACK is either absent or equals FALSE, this function returns', &
'   the position of the leftmost character of STRING that is not in', &
'   SET. If BACK equals TRUE, the rightmost position is returned. If', &
'   all characters of STRING are found in SET, the result is zero.', &
'', &
'ARGUMENTS', &
'  STRING    Shall be of type CHARACTER.', &
'  SET       Shall be of type CHARACTER.', &
'  BACK      (Optional) shall be of type LOGICAL.', &
'  KIND      (Optional) An INTEGER initialization expression', &
'            indicating the kind parameter of the result.', &
'', &
'RETURN VALUE', &
'   The return value is of type INTEGER and of kind KIND. If KIND is', &
'   absent, the return value is of default integer kind.', &
'', &
'EXAMPLE', &
'  Sample program:', &
'', &
'    program demo_verify', &
'    implicit none', &
'    character(len=12):: c1=''Howdy There!''', &
'    character(len=6) :: c2(2)=["Howdy ","there!"]', &
'    character(len=2) :: c3(2)=["de","gh"]', &
'    !=======================================================', &
'    !! LOCATION OF FIRST NONBLANK CHARACTER', &
'    write(*,*)''nonblank '',verify(''  Hello World! '', '' '')', &
'    !! SAME AS LEN_TRIM()', &
'    write(*,*)''length '',verify(''  Hello World!    '', '' '', back = .true.)', &
'    !! ARRAYS', &
'    write(*,*) verify(c1,''de'')                  ! writes 1', &
'    write(*,*) verify(c2,c3)                    ! writes 1 1', &
'    write(*,*) verify(c1,''de'',back=.true.)      ! writes 12', &
'    write(*,*) verify(c2,c3,[.true.,.false.]) ! writes 6 1', &
'    !=======================================================', &
'    write(*,*) verify("fortran", "ao")           ! 1, found ''f''', &
'    write(*,*) verify("fortran", "fo")           ! 3, found ''r''', &
'    write(*,*) verify("fortran", "c++")          ! 1, found ''f''', &
'    write(*,*) verify("fortran", "c++", .true.)  ! 7, found ''n''', &
'    write(*,*) verify("fortran", "nartrof")      ! 0'' found none', &
'    !=======================================================', &
'    !! CHECK IF STRING IS OF FORM NN-HHHHH', &
'    check : block', &
'    logical                    :: lout', &
'    character(len=*),parameter :: int=''0123456789''', &
'    character(len=*),parameter :: hex=''abcdef0123456789''', &
'    character(len=80)          :: chars', &
'', &
'    chars=''32-af43d''', &
'    lout=.true.', &
'    lout = lout.and.(verify(chars(1:2), int) == 0)', &
'    lout = lout.and.(verify(chars(3:3), ''-'') == 0)', &
'    lout = lout.and.(verify(chars(4:8), hex) == 0)', &
'    if(lout)then', &
'       write(*,*)trim(chars),'' passed''', &
'    endif', &
'', &
'    endblock check', &
'    end program demo_verify', &
'', &
'  Results:', &
'', &
'    nonblank            3', &
'    length           14', &
'              1', &
'              1           1', &
'             12', &
'              6           1', &
'              1', &
'              3', &
'              1', &
'              7', &
'              0', &
'    32-af43d passed', &
'', &
'STANDARD', &
'   [[Fortran 95]] and later, with KIND argument [[Fortran 2003]] and later', &
'', &
'CLASS', &
'   Elemental function', &
'', &
'SEE ALSO', &
'   Functions that perform operations on character strings, return lengths', &
'   of arguments, and search for certain arguments:', &
'', &
'   Elemental:     adjustl(3), adjustr(3), index(3), len_trim(3),', &
'                  scan(3), verify(3)', &
'   Nonelemental:  repeat(3), trim(3)', &
'']

shortname="verify"

if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif

case default
   allocate (character(len=256) :: textblock(0))
end select
end function help_intrinsics_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_name(lines)
!@(#) sort_name(3fp):sort strings(a-z) over specified field using shell sort starting with [ character
character(len = *)                :: lines(:)
   character(len = :),allocatable :: ihold
   integer                        :: n, igap, i, j, k, jg
   n = size(lines)
   if(n.gt.0)then
      allocate(character(len = len(lines(1))) :: ihold)
   else
      ihold = ''
   endif
   igap = n
   INFINITE: do
      igap = igap/2
      if(igap.eq.0) exit INFINITE
      k = n-igap
      i = 1
      INNER: do
         j = i
         INSIDE: do
            jg = j+igap
            if( lle( lower(lines(j)), lower(lines(jg)) ) )exit INSIDE
            ihold = lines(j)
            lines(j) = lines(jg)
            lines(jg) = ihold
            j = j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i = i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental pure function lower(str) result (string)
!@(#) M_strings::lower(3f): Changes a string to lowercase over specified range
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)     ! step thru each letter in the string
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32) ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

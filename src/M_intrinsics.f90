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
      add=help_intrinsics_one(cnum) ! get a document by number
      if( size(add) .eq. 0 ) exit
      label=''
      grab=.false.
      is_label=.false.
      ! look for NAME then append everything together till a line starting in column 1 that is all uppercase letters
      ! and assume that is the beginning of the next section to extract the NAME section as one line
      do i=1,size(add)
         if(add(i).eq.'')cycle
            is_label=verify(trim(add(i)),'ABCDEFGHIJKLMNOPQRSTUVWXYZ _') == 0
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
'abs(3fortran)                                                    abs(3fortran)', &
'', &
'NAME', &
'  ABS(3) - [NUMERIC] Absolute value', &
'', &
'SYNOPSIS', &
'  result = abs(a)', &
'', &
'           elemental TYPE(kind=KIND) function abs(a)', &
'', &
'            TYPE(kind=KIND),intent(in) :: a', &
'', &
'CHARACTERISTICS', &
'  o  A may be any real, integer, or complex value.', &
'', &
'  o  If A is complex the returned value will be a real with the same kind as', &
'     A.', &
'', &
'     Otherwise the returned type and kind is the same as for A.', &
'', &
'DESCRIPTION', &
'  ABS(3) computes the absolute value of numeric argument A.', &
'', &
'  In mathematics, the absolute value or modulus of a real number X, denoted', &
'  |X|, is the magnitude of X without regard to its sign.', &
'', &
'  The absolute value of a number may be thought of as its distance from zero.', &
'  So for a complex value the absolute value is a real number with magnitude', &
'  SQRT(X%RE**2,X%IM**2), as if the real component is the x value and the', &
'  imaginary value is the y value for the point <x,y>.', &
'', &
'OPTIONS', &
'  o  A : The value to compute the absolute value of.', &
'', &
'RESULT', &
'  If A is of type integer or real, the value of the result is the absolute', &
'  value |A| and of the same type and kind as the input argument.', &
'', &
'  If A is complex with value (X, Y), the result is a real equal to a', &
'  processor-dependent approximation to', &
'', &
'              sqrt(x**2 + y**2)', &
'', &
'  computed without undue overflow or underflow (that means the computation of', &
'  the result can overflow the allowed magnitude of the real value returned,', &
'  and that very small values can produce underflows if they are squared while', &
'  calculating the returned value, for example).', &
'', &
'  That is, if you think of non-complex values as being complex values on the', &
'  x-axis and complex values as being x-y points <x%re,x%im> the result of', &
'  ABS(3) is the (positive) magnitude of the distance of the value from the', &
'  origin.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_abs', &
'      implicit none', &
'      integer,parameter :: dp=kind(0.0d0)', &
'', &
'      integer           :: i = -1', &
'      real              :: x = -1.0', &
'      complex           :: z = (-3.0,-4.0)', &
'      doubleprecision   :: rr = -45.78_dp', &
'', &
'      character(len=*),parameter :: &', &
'         ! some formats', &
'         frmt  =  ''(1x,a15,1x," In: ",g0,            T51," Out: ",g0)'', &', &
'         frmtc = ''(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'',  &', &
'         g     = ''(*(g0,1x))''', &
'', &
'        ! basic usage', &
'          ! any integer, real, or complex type', &
'          write(*, frmt)  ''integer         '',  i, abs(i)', &
'          write(*, frmt)  ''real            '',  x, abs(x)', &
'          write(*, frmt)  ''doubleprecision '', rr, abs(rr)', &
'          write(*, frmtc) ''complex         '',  z, abs(z)', &
'', &
'        ! You can take the absolute value of any value whose positive value', &
'        ! is representable with the same type and kind.', &
'          write(*, *) ''abs range test : '', abs(huge(0)), abs(-huge(0))', &
'          write(*, *) ''abs range test : '', abs(huge(0.0)), abs(-huge(0.0))', &
'          write(*, *) ''abs range test : '', abs(tiny(0.0)), abs(-tiny(0.0))', &
'          ! A dusty corner is that abs(-huge(0)-1) of an integer would be', &
'          ! a representable negative value on most machines but result in a', &
'          ! positive value out of range.', &
'', &
'        ! elemental', &
'          write(*, g) '' abs is elemental:'', abs([20,  0,  -1,  -3,  100])', &
'', &
'        ! COMPLEX input produces REAL output', &
'          write(*, g)'' complex input produces real output'', &', &
'          & abs(cmplx(30.0_dp,40.0_dp,kind=dp))', &
'          ! dusty corner: "kind=dp" is required or the value returned by', &
'          ! CMPLX() is a default real instead of double precision', &
'', &
'        ! the returned value for complex input can be thought of as the', &
'        ! distance from the origin <0,0>', &
'          write(*, g) '' distance of ('', z, '') from zero is'', abs( z )', &
'          write(*, g) '' so beware of overflow with complex values''', &
'          write(*, g) abs(cmplx( huge(0.0), huge(0.0) ))', &
'          write(*, g) '' because the biggest default real is'',huge(0.0)', &
'', &
'      end program demo_abs', &
'', &
'  Results:', &
'', &
'          integer          In: -1                     Out: 1', &
'          real             In: -1.000000              Out: 1.000000', &
'          doubleprecision  In: -45.78000000000000     Out: 45.78000000000000', &
'          complex          In: (-3.000000,-4.000000)  Out: 5.000000', &
'          abs range test :   2147483647  2147483647', &
'          abs range test :   3.4028235E+38  3.4028235E+38', &
'          abs range test :   1.1754944E-38  1.1754944E-38', &
'          abs is elemental: 20 0 1 3 100', &
'          complex input produces real output 50.00000000000000', &
'          distance of ( -3.000000 -4.000000 ) from zero is 5.000000', &
'          so beware of overflow with complex values', &
'          Inf', &
'          because the biggest default real is .3402823E+39', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  SIGN(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 abs(3fortran)', &
'']

shortname="abs"
call process()


case('2','achar')

textblock=[character(len=256) :: &
'', &
'achar(3fortran)                                                achar(3fortran)', &
'', &
'NAME', &
'  ACHAR(3) - [CHARACTER:CONVERSION] Returns a character in a specified', &
'  position in the ASCII collating sequence', &
'', &
'SYNOPSIS', &
'  result = achar(i [,kind])', &
'', &
'           elemental character(len=1,kind=KIND) function achar(i,KIND)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  The character kind returned is the value of KIND if present.  otherwise,', &
'     a single default character is returned.', &
'', &
'DESCRIPTION', &
'  ACHAR(3) returns the character located at position I (commonly called the', &
'  ADE or ASCII Decimal Equivalent) in the ASCII collating sequence.', &
'', &
'  The ACHAR(3) function is often used for generating in-band escape sequences', &
'  to control terminal attributes, as it makes it easy to print unprintable', &
'  characters such as escape and tab. For example:', &
'', &
'         write(*,''(*(a))'')achar(27),''[2J''', &
'', &
'  will clear the screen on an ANSI-compatible terminal display,', &
'', &
'NOTE', &
'  The ADEs (ASCII Decimal Equivalents) for ASCII are', &
'', &
'      *-------*-------*-------*-------*-------*-------*-------*-------*', &
'      | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|', &
'      | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |', &
'      | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|', &
'      | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |', &
'      | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  '' |', &
'      | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |', &
'      | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |', &
'      | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |', &
'      | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |', &
'      | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |', &
'      | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |', &
'      | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |', &
'      | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |', &
'      |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |', &
'      |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |', &
'      |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|', &
'      *-------*-------*-------*-------*-------*-------*-------*-------*', &
'', &
'OPTIONS', &
'  o  I : the integer value to convert to an ASCII character, in the range 0 to', &
'     127. : ACHAR(3) shall have the value C for any character C capable of', &
'     representation as a default character.', &
'', &
'  o  KIND : a integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'RESULT', &
'  Assuming I has a value in the range 0 <= I <= 127, the result is the', &
'  character in position I of the ASCII collating sequence, provided the', &
'  processor is capable of representing that character in the character kind of', &
'  the result; otherwise, the result is processor dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_achar', &
'      use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64', &
'      implicit none', &
'      integer :: i', &
'         i=65', &
'         write(*,''("decimal     =",i0)'')i', &
'         write(*,''("character   =",a1)'')achar(i)', &
'         write(*,''("binary      =",b0)'')achar(i)', &
'         write(*,''("octal       =",o0)'')achar(i)', &
'         write(*,''("hexadecimal =",z0)'')achar(i)', &
'', &
'         write(*,''(8(i3,1x,a,1x),/)'')(i,achar(i), i=32,126)', &
'', &
'         write(*,''(a)'')upper(''Mixed Case'')', &
'      contains', &
'      ! a classic use of achar(3) is to convert the case of a string', &
'', &
'      pure elemental function upper(str) result (string)', &
'      !', &
'      !$@(#) upper(3f): function to return a trimmed uppercase-only string', &
'      !', &
'      ! input string to convert to all uppercase', &
'      character(*), intent(in)      :: str', &
'      ! output string that contains no miniscule letters', &
'      character(len(str))           :: string', &
'      integer                       :: i, iend', &
'      integer,parameter             :: toupper = iachar(''A'')-iachar(''a'')', &
'         iend=len_trim(str)', &
'         ! initialize output string to trimmed input string', &
'         string = str(:iend)', &
'         ! process each letter in the string', &
'         do concurrent (i = 1:iend)', &
'             select case (str(i:i))', &
'             ! located miniscule letter', &
'             case (''a'':''z'')', &
'                ! change miniscule to majuscule letter', &
'                string(i:i) = achar(iachar(str(i:i))+toupper)', &
'             end select', &
'         enddo', &
'      end function upper', &
'      end program demo_achar', &
'', &
'  Results:', &
'', &
'         decimal     =65', &
'         character   =A', &
'         binary      =1000001', &
'         octal       =101', &
'         hexadecimal =41', &
'          32    33 !  34 "  35 #  36 $  37 %  38 &  39 ''', &
'', &
'          40 (  41 )  42 *  43 +  44 ,  45 -  46 .  47 /', &
'', &
'          48 0  49 1  50 2  51 3  52 4  53 5  54 6  55 7', &
'', &
'          56 8  57 9  58 :  59 ;  60 <  61 =  62 >  63 ?', &
'', &
'          64 @  65 A  66 B  67 C  68 D  69 E  70 F  71 G', &
'', &
'          72 H  73 I  74 J  75 K  76 L  77 M  78 N  79 O', &
'', &
'          80 P  81 Q  82 R  83 S  84 T  85 U  86 V  87 W', &
'', &
'          88 X  89 Y  90 Z  91 [  92 \  93 ]  94 ^  95 _', &
'', &
'          96 `  97 a  98 b  99 c 100 d 101 e 102 f 103 g', &
'', &
'         104 h 105 i 106 j 107 k 108 l 109 m 110 n 111 o', &
'', &
'         112 p 113 q 114 r 115 s 116 t 117 u 118 v 119 w', &
'', &
'         120 x 121 y 122 z 123 { 124 | 125 } 126 ~', &
'', &
'   MIXED CASE', &
'STANDARD', &
'  FORTRAN 77. KIND argument added Fortran 2003', &
'', &
'SEE ALSO', &
'  CHAR(3), IACHAR(3), ICHAR(3)', &
'', &
'RESOURCES', &
'  o  ANSI escape sequences', &
'', &
'  o  M_attr module for controlling ANSI-compatible terminals', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               achar(3fortran)', &
'']

shortname="achar"
call process()


case('3','acos')

textblock=[character(len=256) :: &
'', &
'acos(3fortran)                                                  acos(3fortran)', &
'', &
'NAME', &
'  ACOS(3) - [MATHEMATICS:TRIGONOMETRIC] Arccosine (inverse cosine) function', &
'', &
'SYNOPSIS', &
'  result = acos(x)', &
'', &
'           elemental TYPE(kind=KIND) function acos(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be real or complex', &
'', &
'  o  KIND may be any kind supported by the associated type.', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  ACOS(3) computes the arccosine of X (inverse of COS(X)).', &
'', &
'OPTIONS', &
'  o  X : The value to compute the arctangent of. : If the type is real, the', &
'     value must satisfy |X| <= 1.', &
'', &
'RESULT', &
'  The return value is of the same type and kind as X. The real part of the', &
'  result is in radians and lies in the range 0 <= ACOS(X%RE) <= PI .', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_acos', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128', &
'      implicit none', &
'      character(len=*),parameter :: all=''(*(g0,1x))''', &
'      real(kind=real64) :: x , d2r', &
'', &
'         ! basics', &
'          x = 0.866_real64', &
'          print all,''acos('',x,'') is '', acos(x)', &
'', &
'         ! acos(-1) should be PI', &
'          print all,''for reference &', &
'          &PI ~= 3.14159265358979323846264338327950288419716939937510''', &
'          write(*,*) acos(-1.0_real64)', &
'          d2r=acos(-1.0_real64)/180.0_real64', &
'          print all,''90 degrees is '', d2r*90.0_real64, '' radians''', &
'         ! elemental', &
'          print all,''elemental'',acos([-1.0,-0.5,0.0,0.50,1.0])', &
'         ! complex', &
'          print *,''complex'',acos( (-1.0,  0.0) )', &
'          print *,''complex'',acos( (-1.0, -1.0) )', &
'          print *,''complex'',acos( ( 0.0, -0.0) )', &
'          print *,''complex'',acos( ( 1.0,  0.0) )', &
'', &
'      end program demo_acos', &
'', &
'  Results:', &
'', &
'       acos( 0.86599999999999999 ) is  0.52364958093182890', &
'       for reference PI ~= 3.14159265358979323846264338327950288419716939937510', &
'          3.1415926535897931', &
'       90 degrees is  1.5707963267948966  radians', &
'       elemental 3.14159274 2.09439516 1.57079637 1.04719758 0.00000000', &
'        complex            (3.14159274,-0.00000000)', &
'        complex             (2.23703575,1.06127501)', &
'        complex             (1.57079637,0.00000000)', &
'        complex            (0.00000000,-0.00000000)', &
'', &
'STANDARD', &
'  FORTRAN 77 ; for a complex argument - Fortran 2008', &
'', &
'SEE ALSO', &
'  Inverse function: COS(3)', &
'', &
'RESOURCES', &
'  o  wikipedia: inverse trigonometric functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                acos(3fortran)', &
'']

shortname="acos"
call process()


case('4','acosh')

textblock=[character(len=256) :: &
'', &
'acosh(3fortran)                                                acosh(3fortran)', &
'', &
'NAME', &
'  ACOSH(3) - [MATHEMATICS:TRIGONOMETRIC] Inverse hyperbolic cosine function', &
'', &
'SYNOPSIS', &
'  result = acosh(x)', &
'', &
'           elemental TYPE(kind=KIND) function acosh(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be real or complex', &
'', &
'  o  KIND may be any kind supported by the associated type.', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  ACOSH(3) computes the inverse hyperbolic cosine of X in radians.', &
'', &
'OPTIONS', &
'  o  X : The value to compute the hyperbolic cosine of', &
'', &
'RESULT', &
'  The result has a value equal to a processor-dependent approximation to the', &
'  inverse hyperbolic cosine function of X.', &
'', &
'  If X is complex, the imaginary part of the result is in radians and lies', &
'  between', &
'', &
'    0 <= aimag(acosh(x)) <= PI', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_acosh', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'      implicit none', &
'      real(kind=dp), dimension(3) :: x = [ 1.0d0, 2.0d0, 3.0d0 ]', &
'         write (*,*) acosh(x)', &
'      end program demo_acosh', &
'', &
'  Results:', &
'', &
'       0.000000000000000E+000   1.31695789692482        1.76274717403909', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  Inverse function: COSH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:hyperbolic functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               acosh(3fortran)', &
'']

shortname="acosh"
call process()


case('5','adjustl')

textblock=[character(len=256) :: &
'', &
'adjustl(3fortran)                                            adjustl(3fortran)', &
'', &
'NAME', &
'  ADJUSTL(3) - [CHARACTER:WHITESPACE] Left-justified a string', &
'', &
'SYNOPSIS', &
'  result = adjustl(string)', &
'', &
'         elemental character(len=len(string),kind=KIND) function adjustl(string)', &
'', &
'          character(len=*,kind=KIND),intent(in) :: string', &
'', &
'CHARACTERISTICS', &
'  o  STRING is a character variable of any supported kind', &
'', &
'  o  The return value is a character variable of the same kind and length as', &
'     STRING', &
'', &
'DESCRIPTION', &
'  ADJUSTL(3) will left-justify a string by removing leading spaces. Spaces are', &
'  inserted at the end of the string as needed.', &
'', &
'OPTIONS', &
'  o  STRING : the string to left-justify', &
'', &
'RESULT', &
'  A copy of STRING where leading spaces are removed and the same number of', &
'  spaces are inserted on the end of STRING.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_adjustl', &
'      implicit none', &
'      character(len=20) :: str = ''   sample string''', &
'      character(len=:),allocatable :: astr', &
'      integer :: length', &
'', &
'         ! basic use', &
'          write(*,''(a,"[",a,"]")'') ''original: '',str', &
'          str=adjustl(str)', &
'          write(*,''(a,"[",a,"]")'') ''adjusted: '',str', &
'', &
'          ! a fixed-length string can be printed', &
'          ! trimmed using trim(3f) or len_trim(3f)', &
'          write(*,''(a,"[",a,"]")'') ''trimmed:  '',trim(str)', &
'          length=len_trim(str)', &
'          write(*,''(a,"[",a,"]")'') ''substring:'',str(:length)', &
'', &
'          ! note an allocatable string stays the same length too', &
'          ! and is not trimmed by just an adjustl(3f) call.', &
'          astr=''    allocatable string   ''', &
'          write(*,''(a,"[",a,"]")'') ''original:'',astr', &
'          astr = adjustl(astr)', &
'          write(*,''(a,"[",a,"]")'') ''adjusted:'',astr', &
'          ! trim(3f) can be used to change the length', &
'          astr = trim(astr)', &
'          write(*,''(a,"[",a,"]")'') ''trimmed: '',astr', &
'', &
'      end program demo_adjustl', &
'', &
'  Results:', &
'', &
'         original: [   sample string    ]', &
'         adjusted: [sample string       ]', &
'         trimmed:  [sample string]', &
'         substring:[sample string]', &
'         original:[    allocatable string   ]', &
'         adjusted:[allocatable string       ]', &
'         trimmed: [allocatable string]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ADJUSTR(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             adjustl(3fortran)', &
'']

shortname="adjustl"
call process()


case('6','adjustr')

textblock=[character(len=256) :: &
'', &
'adjustr(3fortran)                                            adjustr(3fortran)', &
'', &
'NAME', &
'  ADJUSTR(3) - [CHARACTER:WHITESPACE] Right-justify a string', &
'', &
'SYNOPSIS', &
'  result = adjustr(string)', &
'', &
'         elemental character(len=len(string),kind=KIND) function adjustr(string)', &
'', &
'          character(len=*,kind=KIND),intent(in) :: string', &
'', &
'CHARACTERISTICS', &
'  o  STRING is a character variable', &
'', &
'  o  The return value is a character variable of the same kind and length as', &
'     STRING', &
'', &
'DESCRIPTION', &
'  ADJUSTR(3) right-justifies a string by removing trailing spaces. Spaces are', &
'  inserted at the start of the string as needed to retain the original length.', &
'', &
'OPTIONS', &
'  o  STRING : the string to right-justify', &
'', &
'RESULT', &
'  Trailing spaces are removed and the same number of spaces are inserted at', &
'  the start of STRING.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_adjustr', &
'      implicit none', &
'      character(len=20) :: str', &
'         ! print a short number line', &
'         write(*,''(a)'')repeat(''1234567890'',2)', &
'', &
'        ! basic usage', &
'         str = ''  sample string ''', &
'         write(*,''(a)'') str', &
'         str = adjustr(str)', &
'         write(*,''(a)'') str', &
'', &
'         !', &
'         ! elemental', &
'         !', &
'         write(*,''(a)'')repeat(''1234567890'',5)', &
'         write(*,''(a)'')adjustr([character(len=50) :: &', &
'         ''  first           '', &', &
'         ''     second       '', &', &
'         ''         third    '' ])', &
'         write(*,''(a)'')repeat(''1234567890'',5)', &
'', &
'      end program demo_adjustr', &
'', &
'  Results:', &
'', &
'         12345678901234567890', &
'           sample string', &
'                sample string', &
'         12345678901234567890123456789012345678901234567890', &
'                                                      first', &
'                                                     second', &
'                                                      third', &
'         12345678901234567890123456789012345678901234567890', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ADJUSTL(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             adjustr(3fortran)', &
'']

shortname="adjustr"
call process()


case('7','aimag')

textblock=[character(len=256) :: &
'', &
'aimag(3fortran)                                                aimag(3fortran)', &
'', &
'NAME', &
'  AIMAG(3) - [TYPE:NUMERIC] Imaginary part of complex number', &
'', &
'SYNOPSIS', &
'  result = aimag(z)', &
'', &
'           elemental complex(kind=KIND) function aimag(z)', &
'', &
'            complex(kind=KIND),intent(in) :: z', &
'', &
'CHARACTERISTICS', &
'  o  The type of the argument Z shall be complex and any supported complex', &
'     kind', &
'', &
'  o  The return value is of type real with the kind type parameter of the', &
'     argument.', &
'', &
'DESCRIPTION', &
'  AIMAG(3) yields the imaginary part of the complex argument Z.', &
'', &
'  This is similar to the modern complex-part-designator %IM which also', &
'  designates the imaginary part of a value, accept a designator can appear on', &
'  the left-hand side of an assignment as well, as in VAL%IM=10.0.', &
'', &
'OPTIONS', &
'  o  Z : The complex value to extract the imaginary component of.', &
'', &
'RESULT', &
'  The return value is a real value with the magnitude and sign of the', &
'  imaginary component of the argument Z.', &
'', &
'  That is, If Z has the value (X,Y), the result has the value Y.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_aimag', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'      implicit none', &
'      character(len=*),parameter :: g=''(*(1x,g0))''', &
'      complex              :: z4', &
'      complex(kind=real64) :: z8', &
'         ! basics', &
'          z4 = cmplx(1.e0, 2.e0)', &
'          print *, ''value='',z4', &
'          print g, ''imaginary part='',aimag(z4),''or'', z4%im', &
'', &
'          ! other kinds other than the default may be supported', &
'          z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)', &
'          print *, ''value='',z8', &
'          print g, ''imaginary part='',aimag(z8),''or'', z8%im', &
'', &
'          ! an elemental function can be passed an array', &
'          print *', &
'          print *, [z4,z4/2.0,z4+z4,z4**3]', &
'          print *', &
'          print *, aimag([z4,z4/2.0,z4+z4,z4**3])', &
'', &
'      end program demo_aimag', &
'', &
'  Results:', &
'', &
'       value= (1.00000000,2.00000000)', &
'       imaginary part= 2.00000000 or 2.00000000', &
'       value= (3.0000000000000000,4.0000000000000000)', &
'       imaginary part= 4.0000000000000000 or 4.0000000000000000', &
'', &
'       (1.00000000,2.00000000) (0.500000000,1.00000000) (2.00000000,4.00000000)', &
'       (-11.0000000,-2.00000000)', &
'', &
'         2.00000000       1.00000000       4.00000000      -2.00000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  CMPLX(3) - Complex conversion function', &
'', &
'  o  CONJG(3) - Complex conjugate function', &
'', &
'  o  REAL(3) - Convert to real type', &
'', &
'  Fortran has strong support for complex values, including many intrinsics', &
'  that take or produce complex values in addition to algebraic and logical', &
'  expressions:', &
'', &
'  ABS(3), ACOSH(3), ACOS(3), ASINH(3), ASIN(3), ATAN2(3), ATANH(3), ATAN(3),', &
'  COSH(3), COS(3), CO_SUM(3), DBLE(3), DOT_PRODUCT(3), EXP(3), INT(3),', &
'  IS_CONTIGUOUS(3), KIND(3), LOG(3), MATMUL(3), PRECISION(3), PRODUCT(3),', &
'  RANGE(3), RANK(3), SINH(3), SIN(3), SQRT(3), STORAGE_SIZE(3), SUM(3),', &
'  TANH(3), TAN(3), UNPACK(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               aimag(3fortran)', &
'']

shortname="aimag"
call process()


case('8','aint')

textblock=[character(len=256) :: &
'', &
'aint(3fortran)                                                  aint(3fortran)', &
'', &
'NAME', &
'  AINT(3) - [NUMERIC] Truncate toward zero to a whole number', &
'', &
'SYNOPSIS', &
'  result = aint(x [,kind])', &
'', &
'           elemental real(kind=KIND) function iaint(x,KIND)', &
'', &
'            real(kind=**),intent(in)   :: x', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  the result is a real of the default kind unless KIND is specified.', &
'', &
'  o  KIND is an integer initialization expression indicating the kind', &
'     parameter of the result.', &
'', &
'DESCRIPTION', &
'  AINT(3) truncates its argument toward zero to a whole number.', &
'', &
'OPTIONS', &
'  o  X : the real value to truncate.', &
'', &
'  o  KIND : indicates the kind parameter of the result.', &
'', &
'RESULT', &
'  The sign is the same as the sign of X unless the magnitude of X is less than', &
'  one, in which case zero is returned.', &
'', &
'  Otherwise AINT(3) returns the largest whole number that does not exceed the', &
'  magnitude of X with the same sign as the input.', &
'', &
'  That is, it truncates the value towards zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_aint', &
'      use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64', &
'      implicit none', &
'      real(kind=dp) :: x8', &
'         print *,''basics:''', &
'         print *,'' just chops off the fractional part''', &
'         print *,  aint(-2.999), aint(-2.1111)', &
'         print *,'' if |x| < 1 a positive zero is returned''', &
'         print *,  aint(-0.999), aint( 0.9999)', &
'         print *,'' input may be of any real kind''', &
'         x8 = 4.3210_dp', &
'         print *, aint(-x8), aint(x8)', &
'         print *,''elemental:''', &
'         print *,aint([ &', &
'          &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'          &  0.0,   &', &
'          &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'      end program demo_aint', &
'', &
'  Results:', &
'', &
'       basics:', &
'        just chops off the fractional part', &
'        -2.000000      -2.000000', &
'        if |x| < 1 a positive zero is returned', &
'        0.0000000E+00  0.0000000E+00', &
'        input may be of any real kind', &
'        -4.00000000000000        4.00000000000000', &
'       elemental:', &
'        -2.000000      -2.000000      -2.000000      -2.000000      -1.000000', &
'        -1.000000      0.0000000E+00  0.0000000E+00  0.0000000E+00   1.000000', &
'         1.000000       2.000000       2.000000       2.000000       2.000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ANINT(3), INT(3), NINT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                aint(3fortran)', &
'']

shortname="aint"
call process()


case('9','all')

textblock=[character(len=256) :: &
'', &
'all(3fortran)                                                    all(3fortran)', &
'', &
'NAME', &
'  ALL(3) - [ARRAY:REDUCTION] Determines if all the values are true', &
'', &
'SYNOPSIS', &
'  result = all(mask [,dim])', &
'', &
'           function all(mask ,dim)', &
'', &
'            logical(kind=KIND),intent(in) :: mask(..)', &
'            integer,intent(in),optional   :: dim', &
'            logical(kind=KIND)            :: all(..)', &
'', &
'CHARACTERISTICS', &
'  o  MASK is a logical array', &
'', &
'  o  DIM is an integer', &
'', &
'  o  the result is a logical array if DIM is supplied, otherwise it is a', &
'     logical scalar. It has the same characteristics as MASK', &
'', &
'DESCRIPTION', &
'  ALL(3) determines if all the values are true in MASK in the array along', &
'  dimension DIM if DIM is specified; otherwise all elements are tested', &
'  together.', &
'', &
'  This testing type is called a logical conjunction of elements of MASK along', &
'  dimension DIM.', &
'', &
'  The mask is generally a logical expression, allowing for comparing arrays', &
'  and many other common operations.', &
'', &
'OPTIONS', &
'  o  MASK : the logical array to be tested for all elements being .true.', &
'', &
'  o  DIM : DIM indicates the direction through the elements of MASK to group', &
'     elements for testing. : DIM has a value that lies between one and the', &
'     rank of MASK. : The corresponding actual argument shall not be an', &
'     optional dummy argument. : If DIM is not present all elements are tested', &
'     and a single scalar value is returned.', &
'', &
'RESULT', &
'  1.  If DIM is not present ALL(MASK) is .true. if all elements of MASK are', &
'      .true.. It also is .true. if MASK has zero size; otherwise, it is', &
'      .false. .', &
'', &
'  2.  If the rank of MASK is one, then ALL(MASK, DIM) is equivalent to', &
'      ALL(MASK).', &
'', &
'  3.  If the rank of MASK is greater than one and DIM is present then', &
'      ALL(MASK,DIM) returns an array with the rank (number of dimensions) of', &
'      MASK minus 1. The shape is determined from the shape of MASK where the', &
'      DIM dimension is elided. A value is returned for each set of elements', &
'      along the DIM dimension.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_all', &
'      implicit none', &
'      logical,parameter :: T=.true., F=.false.', &
'      logical bool', &
'        ! basic usage', &
'         ! is everything true?', &
'         bool = all([ T,T,T ])', &
'         bool = all([ T,F,T ])', &
'         print *, bool', &
'', &
'        ! by a dimension', &
'         ARRAYS: block', &
'         integer :: a(2,3), b(2,3)', &
'          ! set everything to one except one value in b', &
'          a = 1', &
'          b = 1', &
'          b(2,2) = 2', &
'          ! now compare those two arrays', &
'          print *,''entire array :'', all(a ==  b )', &
'          print *,''compare columns:'', all(a ==  b, dim=1)', &
'          print *,''compare rows:'', all(a ==  b, dim=2)', &
'        end block ARRAYS', &
'', &
'      end program demo_all', &
'', &
'  Results:', &
'', &
'       >  T', &
'       >  F', &
'       >  entire array : F', &
'       >  compare columns: T F T', &
'       >  compare rows: T F', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ANY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 all(3fortran)', &
'']

shortname="all"
call process()


case('10','allocated')

textblock=[character(len=256) :: &
'', &
'allocated(3fortran)                                        allocated(3fortran)', &
'', &
'NAME', &
'  ALLOCATED(3) - [ARRAY:INQUIRY] Allocation status of an allocatable entity', &
'', &
'SYNOPSIS', &
'  result = allocated(array|scalar)', &
'', &
'           logical function allocated(array,scalar)', &
'', &
'            type(TYPE(kind=**)),allocatable,optional :: array(..)', &
'            type(TYPE(kind=**)),allocatable,optional :: scalar', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  ARRAY may be any allocatable array object of any type.', &
'', &
'  o  SCALAR may be any allocatable scalar of any type.', &
'', &
'  o  the result is a default logical scalar', &
'', &
'DESCRIPTION', &
'  ALLOCATED(3) checks the allocation status of both arrays and scalars.', &
'', &
'  At least one and only one of ARRAY or SCALAR must be specified.', &
'', &
'OPTIONS', &
'  o  ENTITY : the allocatable object to test.', &
'', &
'RESULT', &
'  If the argument is allocated then the result is .true.; otherwise, it', &
'  returns .false..', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_allocated', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'      implicit none', &
'      real(kind=sp), allocatable :: x(:)', &
'      character(len=256) :: message', &
'      integer :: istat', &
'        ! basics', &
'         if( allocated(x)) then', &
'             write(*,*)''do things if allocated''', &
'         else', &
'             write(*,*)''do things if not allocated''', &
'         endif', &
'', &
'         ! if already allocated, deallocate', &
'         if ( allocated(x) ) deallocate(x,STAT=istat, ERRMSG=message )', &
'         if(istat.ne.0)then', &
'            write(*,*)trim(message)', &
'            stop', &
'         endif', &
'', &
'         ! only if not allocated, allocate', &
'         if ( .not. allocated(x) ) allocate(x(20))', &
'', &
'        ! allocation and intent(out)', &
'         call intentout(x)', &
'         write(*,*)''note it is deallocated!'',allocated(x)', &
'', &
'         contains', &
'', &
'         subroutine intentout(arr)', &
'         ! note that if arr has intent(out) and is allocatable,', &
'         ! arr is deallocated on entry', &
'         real(kind=sp),intent(out),allocatable :: arr(:)', &
'             write(*,*)''note it was allocated in calling program'',allocated(arr)', &
'         end subroutine intentout', &
'', &
'      end program demo_allocated', &
'', &
'  Results:', &
'', &
'       >  do things if not allocated', &
'       >  note it was allocated in calling program F', &
'       >  note it is deallocated! F', &
'', &
'STANDARD', &
'  Fortran 95. allocatable scalar entities were added in Fortran 2003.', &
'', &
'SEE ALSO', &
'  MOVE_ALLOC(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022           allocated(3fortran)', &
'']

shortname="allocated"
call process()


case('11','anint')

textblock=[character(len=256) :: &
'', &
'anint(3fortran)                                                anint(3fortran)', &
'', &
'NAME', &
'  ANINT(3) - [NUMERIC] Real nearest whole number', &
'', &
'SYNOPSIS', &
'  result = anint(a [,kind])', &
'', &
'           elemental real(kind=KIND) function anint(x,KIND)', &
'', &
'            real(kind=**),intent(in)   :: x', &
'            integer,intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  A is type real of any kind', &
'', &
'  o  KIND is a scalar integer constant expression.', &
'', &
'  o  the result is type real. The kind of the result is the same as X unless', &
'     specified by KIND.', &
'', &
'DESCRIPTION', &
'  ANINT(3) rounds its argument to the nearest whole number.', &
'', &
'  Unlike NINT(3) which returns an integer the full range or real values can be', &
'  returned (integer types typically have a smaller range of values than real', &
'  types).', &
'', &
'OPTIONS', &
'  o  A : the value to round', &
'', &
'  o  KIND : specifies the kind of the result. The default is the kind of A.', &
'', &
'RESULT', &
'  The return value is the real whole number nearest A.', &
'', &
'  If A is greater than zero, ANINT(A)(3) returns AINT(A + 0.5).', &
'', &
'  If A is less than or equal to zero then it returns AINT(A - 0.5), except', &
'  AINT specifies that for |A| < 1 the result is zero (0).', &
'', &
'  It is processor-dependent whether anint(a) returns negative zero when -0.5 <', &
'  a <= -0.0. Compiler switches are often available which enable or disable', &
'  support of negative zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_anint', &
'      use, intrinsic :: iso_fortran_env, only : real32, real64, real128', &
'      implicit none', &
'      real,allocatable :: arr(:)', &
'', &
'        ! basics', &
'         print *, ''ANINT (2.783) has the value 3.0 =>'', anint(2.783)', &
'         print *, ''ANINT (-2.783) has the value -3.0 =>'', anint(-2.783)', &
'', &
'         print *, ''by default the kind of the output is the kind of the input''', &
'         print *, anint(1234567890.1234567890e0)', &
'         print *, anint(1234567890.1234567890d0)', &
'', &
'         print *, ''sometimes specifying the result kind is useful when passing''', &
'         print *, ''results as an argument, for example.''', &
'         print *, ''do you know why the results are different?''', &
'         print *, anint(1234567890.1234567890,kind=real64)', &
'         print *, anint(1234567890.1234567890d0,kind=real64)', &
'', &
'        ! elemental', &
'         print *, ''numbers on a cusp are always the most troublesome''', &
'         print *, anint([ -2.7, -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, 0.0 ])', &
'', &
'         print *, ''negative zero is processor dependent''', &
'         arr=[ 0.0, 0.1, 0.5, 1.0, 1.5, 2.0, 2.2, 2.5, 2.7 ]', &
'         print *, anint(arr)', &
'         arr=[ -0.0, -0.1, -0.5, -1.0, -1.5, -2.0, -2.2, -2.5, -2.7 ]', &
'         print *, anint(arr)', &
'', &
'      end program demo_anint', &
'', &
'  Results:', &
'', &
'       >  ANINT (2.783) has the value 3.0 =>   3.000000', &
'       >  ANINT (-2.783) has the value -3.0 =>  -3.000000', &
'       >  by default the kind of the output is the kind of the input', &
'       >   1.2345679E+09', &
'       >    1234567890.00000', &
'       >  sometimes specifying the result kind is useful when passing', &
'       >  results as an argument, for example.', &
'       >  do you know why the results are different?', &
'       >    1234567936.00000', &
'       >    1234567890.00000', &
'       >  numbers on a cusp are always the most troublesome', &
'       >   -3.000000      -3.000000      -2.000000      -2.000000      -2.000000', &
'       >   -1.000000      -1.000000      0.0000000E+00', &
'       >  negative zero is processor dependent', &
'       >   0.0000000E+00  0.0000000E+00   1.000000       1.000000       2.000000', &
'       >    2.000000       2.000000       3.000000       3.000000', &
'       >   0.0000000E+00  0.0000000E+00  -1.000000      -1.000000      -2.000000', &
'       >   -2.000000      -2.000000      -3.000000      -3.000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  AINT(3), INT(3), NINT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               anint(3fortran)', &
'']

shortname="anint"
call process()


case('12','any')

textblock=[character(len=256) :: &
'', &
'any(3fortran)                                                    any(3fortran)', &
'', &
'NAME', &
'  ANY(3) - [ARRAY:REDUCTION] Determines if any of the values in the logical', &
'  array are .true.', &
'', &
'SYNOPSIS', &
'  result = any(mask [,dim])', &
'', &
'           function any(mask, dim)', &
'', &
'            logical(kind=KIND),intent(in) :: mask(..)', &
'            integer,intent(in),optional   :: dim', &
'            logical(kind=KIND)            :: any(..)', &
'', &
'CHARACTERISTICS', &
'  o  MASK is a logical array', &
'', &
'  o  DIM is a scalar integer', &
'', &
'  o  the result is a logical array if DIM is supplied, otherwise it is a', &
'     logical scalar.', &
'', &
'DESCRIPTION', &
'  ANY(3) determines if any of the values in the logical array MASK along', &
'  dimension DIM are .true..', &
'', &
'OPTIONS', &
'  o  MASK : an array of logical expressions or values to be tested in groups', &
'     or in total for a .true. value.', &
'', &
'  o  DIM : a whole number value that lies between one and RANK(MASK) that', &
'     indicates to return an array of values along the indicated dimension', &
'     instead of a scalar answer.', &
'', &
'RESULT', &
'  ANY(MASK) returns a scalar value of type logical where the kind type', &
'  parameter is the same as the kind type parameter of MASK. If DIM is present,', &
'  then ANY(MASK, DIM) returns an array with the rank of MASK minus 1. The', &
'  shape is determined from the shape of MASK where the DIM dimension is', &
'  elided.', &
'', &
'  1.  ANY(MASK) is .true. if any element of MASK is .true.; otherwise, it is', &
'      .false.. It also is .false. if MASK has zero size.', &
'', &
'  2.  If the rank of MASK is one, then ANY(MASK, DIM) is equivalent to', &
'      ANY(MASK). If the rank is greater than one, then ANY(MASK, DIM) is', &
'      determined by applying ANY(MASK) to the array sections.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_any', &
'      implicit none', &
'      logical,parameter :: T=.true., F=.false.', &
'      integer           :: a(2,3), b(2,3)', &
'      logical           :: bool', &
'        ! basic usage', &
'         bool = any([F,F,T,F])', &
'         print *,bool', &
'         bool = any([F,F,F,F])', &
'         print *,bool', &
'        ! fill two integer arrays with values for testing', &
'         a = 1', &
'         b = 1', &
'         b(:,2) = 2', &
'         b(:,3) = 3', &
'        ! using any(3) with logical expressions you can compare two arrays', &
'        ! in a myriad of ways', &
'         ! first, print where elements of b are bigger than in a', &
'         call printl( ''first print b > a             '', b > a         )', &
'         ! now use any() to test', &
'         call printl( ''any true values?  any(b > a)  '', any(b > a )   )', &
'         call printl( ''again by columns? any(b > a,1)'', any(b > a, 1) )', &
'         call printl( ''again by rows?    any(b > a,2)'', any(b > a, 2) )', &
'      contains', &
'      ! CONVENIENCE ROUTINE. this is not specific to ANY()', &
'      subroutine printl(title,a)', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'       & stderr=>ERROR_UNIT,&', &
'       & stdin=>INPUT_UNIT,&', &
'       & stdout=>OUTPUT_UNIT', &
'      implicit none', &
'', &
'      !@(#) print small 2d logical scalar, vector, or matrix', &
'', &
'      character(len=*),parameter   :: all=''(*(g0,1x))''', &
'      character(len=*),parameter   :: row=''(" > [ ",*(l1:,","))''', &
'      character(len=*),intent(in)  :: title', &
'      logical,intent(in)           :: a(..)', &
'      integer                      :: i', &
'         write(*,*)', &
'         write(*,all,advance=''no'')trim(title),&', &
'          & '' : shape='',shape(a),'',rank='',rank(a),'',size='',size(a)', &
'         ! get size and shape of input', &
'         select rank(a)', &
'         rank (0); write(*,''(a)'')''(a scalar)''', &
'            write(*,fmt=row,advance=''no'')a', &
'            write(*,''(" ]")'')', &
'         rank (1); write(*,''(a)'')''(a vector)''', &
'            do i=1,size(a)', &
'               write(*,fmt=row,advance=''no'')a(i)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'         rank (2); write(*,''(a)'')''(a matrix) ''', &
'            do i=1,size(a,dim=1)', &
'               write(*,fmt=row,advance=''no'')a(i,:)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'         rank default', &
'            write(stderr,*)''*printl* did not expect rank='', rank(a), &', &
'             & ''shape='', shape(a),''size='',size(a)', &
'            stop ''*printl* unexpected rank''', &
'         end select', &
'', &
'      end subroutine printl', &
'', &
'      end program demo_any', &
'', &
'  Results:', &
'', &
'       >  T', &
'       >  F', &
'       >', &
'       > first print b > a : shape=23,rank=2,size=6(a matrix)', &
'       >  > [ F,T,T ]', &
'       >  > [ F,T,T ]', &
'       >', &
'       > any true values?  any(b > a) : shape=,rank=0,size=1(a scalar)', &
'       >  > [ T ]', &
'       >', &
'       > again by columns? any(b > a,1) : shape=3,rank=1,size=3(a vector)', &
'       >  > [ F ]', &
'       >  > [ T ]', &
'       >  > [ T ]', &
'       >', &
'       > again by rows?    any(b > a,2) : shape=2,rank=1,size=2(a vector)', &
'       >  > [ T ]', &
'       >  > [ T ]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ALL(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 any(3fortran)', &
'']

shortname="any"
call process()


case('13','asin')

textblock=[character(len=256) :: &
'', &
'asin(3fortran)                                                  asin(3fortran)', &
'', &
'NAME', &
'  ASIN(3) - [MATHEMATICS:TRIGONOMETRIC] Arcsine function', &
'', &
'SYNOPSIS', &
'  result = asin(x)', &
'', &
'           elemental TYPE(kind=KIND) function asin(x)', &
'', &
'            TYPE(kind=KIND) :: x', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be real or complex', &
'', &
'  o  KIND may be any kind supported by the associated type.', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  ASIN(3) computes the arcsine of its argument X.', &
'', &
'  The arcsine is the inverse function of the sine function. It is commonly', &
'  used in trigonometry when trying to find the angle when the lengths of the', &
'  hypotenuse and the opposite side of a right triangle are known.', &
'', &
'OPTIONS', &
'  o  X : The value to compute the arcsine of : The type shall be either real', &
'     and a magnitude that is less than or equal to one; or be complex.', &
'', &
'RESULT', &
'  o  RESULT The result has a value equal to a processor-dependent', &
'     approximation to arcsin(x).', &
'', &
'     If X is real the result is real and it is expressed in radians and lies', &
'     in the range', &
'', &
'                 PI/2 <= ASIN (X) <= PI/2.', &
'', &
'     If the argument (and therefore the result) is imaginary the real part of', &
'     the result is in radians and lies in the range', &
'', &
'             -PI/2 <= real(asin(x)) <= PI/2', &
'', &
'EXAMPLES', &
'  The arcsine will allow you to find the measure of a right angle when you', &
'  know the ratio of the side opposite the angle to the hypotenuse.', &
'', &
'  So if you knew that a train track rose 1.25 vertical miles on a track that', &
'  was 50 miles long, you could determine the average angle of incline of the', &
'  track using the arcsine. Given', &
'', &
'       sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)', &
'', &
'  Sample program:', &
'', &
'      program demo_asin', &
'      use, intrinsic :: iso_fortran_env, only : dp=>real64', &
'      implicit none', &
'      ! value to convert degrees to radians', &
'      real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp', &
'      real(kind=dp)           :: angle, rise, run', &
'      character(len=*),parameter :: all=''(*(g0,1x))''', &
'        ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)', &
'        ! then taking the arcsine of both sides of the equality yields', &
'        ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)', &
'        rise=1.250_dp', &
'        run=50.00_dp', &
'        angle = asin(rise/run)', &
'        print all, ''angle of incline(radians) = '', angle', &
'        angle = angle/D2R', &
'        print all, ''angle of incline(degrees) = '', angle', &
'', &
'        print all, ''percent grade='',rise/run*100.0_dp', &
'      end program demo_asin', &
'', &
'  Results:', &
'', &
'          angle of incline(radians) =    2.5002604899361139E-002', &
'          angle of incline(degrees) =    1.4325437375665075', &
'          percent grade=   2.5000000000000000', &
'', &
'  The percentage grade is the slope, written as a percent. To calculate the', &
'  slope you divide the rise by the run. In the example the rise is 1.25 mile', &
'  over a run of 50 miles so the slope is 1.25/50 = 0.025.  Written as a', &
'  percent this is 2.5 %.', &
'', &
'  For the US, two and 1/2 percent is generally thought of as the upper limit.', &
'  This means a rise of 2.5 feet when going 100 feet forward. In the US this', &
'  was the maximum grade on the first major US railroad, the Baltimore and', &
'  Ohio. Note curves increase the frictional drag on a train reducing the', &
'  allowable grade.', &
'', &
'STANDARD', &
'  FORTRAN 77 , for a complex argument Fortran 2008', &
'', &
'SEE ALSO', &
'  Inverse function: SIN(3)', &
'', &
'RESOURCES', &
'  o  wikipedia: inverse trigonometric functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                asin(3fortran)', &
'']

shortname="asin"
call process()


case('14','asinh')

textblock=[character(len=256) :: &
'', &
'asinh(3fortran)                                                asinh(3fortran)', &
'', &
'NAME', &
'  ASINH(3) - [MATHEMATICS:TRIGONOMETRIC] Inverse hyperbolic sine function', &
'', &
'SYNOPSIS', &
'  result = asinh(x)', &
'', &
'           elemental TYPE(kind=KIND) function asinh(x)', &
'', &
'            TYPE(kind=KIND) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be any real or complex type', &
'', &
'  o  KIND may be any kind supported by the associated type', &
'', &
'  o  The returned value will be of the same type and kind as the argument', &
'', &
'   X', &
'DESCRIPTION', &
'  ASINH(3) computes the inverse hyperbolic sine of X.', &
'', &
'OPTIONS', &
'  o  X : The value to compute the inverse hyperbolic sine of', &
'', &
'RESULT', &
'  The result has a value equal to a processor-dependent approximation to the', &
'  inverse hyperbolic sine function of X.', &
'', &
'  If X is complex, the imaginary part of the result is in radians and lies', &
'  between -PI/2 <= AIMAG(ASINH(X)) <= PI/2.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_asinh', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'      implicit none', &
'      real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]', &
'', &
'         ! elemental', &
'          write (*,*) asinh(x)', &
'', &
'      end program demo_asinh', &
'', &
'  Results:', &
'', &
'        -0.88137358701954305  0.0000000000000000  0.88137358701954305', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  Inverse function: SINH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:hyperbolic functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               asinh(3fortran)', &
'']

shortname="asinh"
call process()


case('15','associated')

textblock=[character(len=256) :: &
'', &
'associated(3fortran)                                      associated(3fortran)', &
'', &
'NAME', &
'  ASSOCIATED(3) - [STATE:INQUIRY] Association status of a pointer or', &
'  pointer/target pair', &
'', &
'SYNOPSIS', &
'  result = associated(pointer [,target])', &
'', &
'           logical function associated(pointer,target)', &
'', &
'            type(TYPE(kind=KIND),pointer :: pointer', &
'            type(TYPE(kind=KIND),pointer,optional :: target', &
'', &
'CHARACTERISTICS', &
'  o  POINTER shall have the pointer attribute and it can be any type or may be', &
'     a procedure pointer', &
'', &
'  o  TARGET shall be a pointer or a target. It must have the same type, kind', &
'     type parameter, and array rank as POINTER.', &
'', &
'  o  The association status of neither POINTER nor TARGET shall be undefined.', &
'', &
'  o  the result is a default logical value', &
'', &
'DESCRIPTION', &
'  ASSOCIATED(3) determines the status of the pointer POINTER or if POINTER is', &
'  associated with the target TARGET.', &
'', &
'OPTIONS', &
'  o  POINTER : A pointer to test for association. Its pointer association', &
'     status shall not be undefined.', &
'', &
'  o  TARGET : A target that is to be tested for occupying the same storage', &
'     units as the pointer POINTER. That is, it is tested as to whether it is', &
'     pointed to by POINTER.', &
'', &
'RESULT', &
'  ASSOCIATED(3f) returns a scalar value of type logical. There are several', &
'  cases:', &
'', &
'  1.  When the optional TARGET is not present then ASSOCIATED(POINTER) is', &
'      .true. if POINTER is associated with a target; otherwise, it returns', &
'      .false..', &
'', &
'  2.  If TARGET is present and a scalar target, the result is .true. if TARGET', &
'      is not a zero-sized storage sequence and the target associated with', &
'      POINTER occupies the same storage units. If POINTER is disassociated,', &
'      the result is .false..', &
'', &
'  3.  If TARGET is present and an array target, the result is .true. if TARGET', &
'      and POINTER have the same shape, are not zero-sized arrays, are arrays', &
'      whose elements are not zero-sized storage sequences, and TARGET and', &
'      POINTER occupy the same storage units in array element order.', &
'', &
'      As in case 2, the result is .false., if POINTER is disassociated.', &
'', &
'  4.  If TARGET is present and an scalar pointer, the result is .true. if', &
'      TARGET is associated with POINTER, the target associated with TARGET are', &
'      not zero-sized storage sequences and occupy the same storage units.', &
'', &
'      The result is .false., if either TARGET or POINTER is disassociated.', &
'', &
'  5.  If TARGET is present and an array pointer, the result is .true. if', &
'      target associated with POINTER and the target associated with TARGET', &
'      have the same shape, are not zero-sized arrays, are arrays whose', &
'      elements are not zero-sized storage sequences, and TARGET and POINTER', &
'      occupy the same storage units in array element order.', &
'', &
'  6.  If TARGET is present and is a procedure, the result is true if and only', &
'      if POINTER is associated with TARGET and, if TARGET is an internal', &
'      procedure, they have the same host instance.', &
'', &
'  7.  If TARGET is present and is a procedure pointer, the result is true if', &
'      and only if POINTER and TARGET are associated with the same procedure', &
'      and, if the procedure is an internal procedure, they have the same host', &
'      instance.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_associated', &
'      implicit none', &
'      real, target  :: tgt(2) = [1., 2.]', &
'      real, pointer :: ptr(:)', &
'         ptr => tgt', &
'         if (associated(ptr)     .eqv. .false.) &', &
'         & stop ''POINTER NOT ASSOCIATED''', &
'         if (associated(ptr,tgt) .eqv. .false.) &', &
'         & stop ''POINTER NOT ASSOCIATED TO TARGET''', &
'      end program demo_associated', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  NULL(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022          associated(3fortran)', &
'']

shortname="associated"
call process()


case('16','atan2')

textblock=[character(len=256) :: &
'', &
'atan2(3fortran)                                                atan2(3fortran)', &
'', &
'NAME', &
'  ATAN2(3) - [MATHEMATICS:TRIGONOMETRIC] Arctangent (inverse tangent) function', &
'', &
'SYNOPSIS', &
'  result = atan2(y, x)', &
'', &
'           elemental real(kind=KIND) function atan2(y, x)', &
'', &
'            real,kind=KIND) :: atan2', &
'            real,kind=KIND),intent(in) :: y, x', &
'', &
'CHARACTERISTICS', &
'  o  X and Y must be reals of the same kind.', &
'', &
'  o  The return value has the same type and kind as Y and X.', &
'', &
'DESCRIPTION', &
'  ATAN2(3) computes in radians a processor-dependent approximation of the', &
'  arctangent of the complex number ( X, Y ) or equivalently the principal', &
'  value of the arctangent of the value Y/X (which determines a unique angle).', &
'', &
'  If Y has the value zero, X shall not have the value zero.', &
'', &
'  The resulting phase lies in the range -PI <= ATAN2 (Y,X) <= PI and is equal', &
'  to a processor-dependent approximation to a value of arctan(Y/X).', &
'', &
'OPTIONS', &
'  o  Y : The imaginary component of the complex value (X,Y) or the Y component', &
'     of the point <X,Y>.', &
'', &
'  o  X : The real component of the complex value (X,Y) or the X component of', &
'     the point <X,Y>.', &
'', &
'RESULT', &
'  The value returned is by definition the principal value of the complex', &
'  number (X, Y), or in other terms, the phase of the phasor x+i*y.', &
'', &
'  The principal value is simply what we get when we adjust a radian value to', &
'  lie between -PI and PI inclusive,', &
'', &
'  The classic definition of the arctangent is the angle that is formed in', &
'  Cartesian coordinates of the line from the origin point <0,0> to the point', &
'  <X,Y> .', &
'', &
'  Pictured as a vector it is easy to see that if X and Y are both zero the', &
'  angle is indeterminate because it sits directly over the origin, so', &
'  ATAN(0.0,0.0) will produce an error.', &
'', &
'  Range of returned values by quadrant:', &
'', &
'      >                   +PI/2', &
'      >                     |', &
'      >                     |', &
'      >     PI/2 < z < PI   |   0 > z < PI/2', &
'      >                     |', &
'      >   +-PI -------------+---------------- +-0', &
'      >                     |', &
'      >     PI/2 < -z < PI  |   0 < -z < PI/2', &
'      >                     |', &
'      >                     |', &
'      >                   -PI/2', &
'      >', &
'           NOTES:', &
'', &
'           If the processor distinguishes -0 and +0 then the sign of the', &
'           returned value is that of Y when Y is zero, else when Y is zero', &
'           the returned value is always positive.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atan2', &
'      real :: z', &
'      complex :: c', &
'', &
'       ! basic usage', &
'        ! ATAN2 (1.5574077, 1.0) has the value 1.0 (approximately).', &
'        z=atan2(1.5574077, 1.0)', &
'        write(*,*) ''radians='',z,''degrees='',r2d(z)', &
'', &
'       ! elemental arrays', &
'        write(*,*)''elemental'',atan2( [10.0, 20.0], [30.0,40.0] )', &
'', &
'       ! elemental arrays and scalars', &
'        write(*,*)''elemental'',atan2( [10.0, 20.0], 50.0 )', &
'', &
'       ! break complex values into real and imaginary components', &
'       ! (note TAN2() can take a complex type value )', &
'        c=(0.0,1.0)', &
'        write(*,*)''complex'',c,atan2( x=c%re, y=c%im )', &
'', &
'       ! extended sample converting cartesian coordinates to polar', &
'        COMPLEX_VALS: block', &
'        real                :: ang, radius', &
'        complex,allocatable :: vals(:)', &
'', &
'        vals=[ &', &
'          ( 1.0, 0.0 ), & ! 0', &
'          ( 1.0, 1.0 ), & ! 45', &
'          ( 0.0, 1.0 ), & ! 90', &
'          (-1.0, 1.0 ), & ! 135', &
'          (-1.0, 0.0 ), & ! 180', &
'          (-1.0,-1.0 ), & ! 225', &
'          ( 0.0,-1.0 )]   ! 270', &
'        do i=1,size(vals)', &
'           call cartesian_to_polar(vals(i)%re, vals(i)%im, radius,ang)', &
'           write(*,101)vals(i),ang,r2d(ang),radius', &
'        enddo', &
'        101 format(             &', &
'        & ''X= '',f5.2,           &', &
'        & '' Y= '',f5.2,          &', &
'        & '' ANGLE= '',g0,        &', &
'        & T38,''DEGREES= '',g0.4, &', &
'        & T54,''DISTANCE='',g0)', &
'       endblock COMPLEX_VALS', &
'', &
'      contains', &
'', &
'      elemental real function r2d(radians)', &
'      ! input radians to convert to degrees', &
'      doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians', &
'      real,intent(in)           :: radians', &
'         r2d=radians / DEGREE ! do the conversion', &
'      end function r2d', &
'', &
'      subroutine cartesian_to_polar(x,y,radius,inclination)', &
'      ! return angle in radians in range 0 to 2*PI', &
'      implicit none', &
'      real,intent(in)  :: x,y', &
'      real,intent(out) :: radius,inclination', &
'         radius=sqrt(x**2+y**2)', &
'         if(radius.eq.0)then', &
'            inclination=0.0', &
'         else', &
'            inclination=atan2(y,x)', &
'            if(inclination < 0.0)inclination=inclination+2*atan2(0.0d0,-1.0d0)', &
'         endif', &
'      end subroutine cartesian_to_polar', &
'', &
'      end program demo_atan2', &
'', &
'  Results:', &
'', &
'       >  radians=   1.000000     degrees=   57.29578', &
'       >  elemental  0.3217506      0.4636476', &
'       >  elemental  0.1973956      0.3805064', &
'       >  complex (0.0000000E+00,1.000000)   1.570796', &
'       > X=  1.00 Y=  0.00 ANGLE= .000000     DEGREES= .000   DISTANCE=1.000000', &
'       > X=  1.00 Y=  1.00 ANGLE= .7853982    DEGREES= 45.00  DISTANCE=1.414214', &
'       > X=  0.00 Y=  1.00 ANGLE= 1.570796    DEGREES= 90.00  DISTANCE=1.000000', &
'       > X= -1.00 Y=  1.00 ANGLE= 2.356194    DEGREES= 135.0  DISTANCE=1.414214', &
'       > X= -1.00 Y=  0.00 ANGLE= 3.141593    DEGREES= 180.0  DISTANCE=1.000000', &
'       > X= -1.00 Y= -1.00 ANGLE= 3.926991    DEGREES= 225.0  DISTANCE=1.414214', &
'       > X=  0.00 Y= -1.00 ANGLE= 4.712389    DEGREES= 270.0  DISTANCE=1.000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  ATAN(3)', &
'', &
'RESOURCES', &
'  o  arctan:wikipedia fortran-lang intrinsic descriptions (license: MIT)', &
'     @urbanjost', &
'', &
'                               December 16, 2022               atan2(3fortran)', &
'']

shortname="atan2"
call process()


case('17','atan')

textblock=[character(len=256) :: &
'', &
'atan(3fortran)                                                  atan(3fortran)', &
'', &
'NAME', &
'  ATAN(3) - [MATHEMATICS:TRIGONOMETRIC] Arctangent AKA inverse tangent', &
'  function', &
'', &
'SYNOPSIS', &
'  result = atan([x) | atan(y, x)', &
'', &
'           elemental TYPE(kind=KIND) function atan(y,x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'            TYPE(kind=**),intent(in),optional :: y', &
'', &
'CHARACTERISTICS', &
'  o  If Y is present X and Y must both be real. Otherwise, X may be complex.', &
'', &
'  o  KIND can be any kind supported by the associated type.', &
'', &
'  o  The returned value is of the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  ATAN(3) computes the arctangent of X.', &
'', &
'OPTIONS', &
'  o  X : The value to compute the arctangent of. if Y is present, X shall be', &
'     real.', &
'', &
'  o  Y : is of the same type and kind as X. If X is zero, Y must not be zero.', &
'', &
'RESULT', &
'  The returned value is of the same type and kind as X. If Y is present, the', &
'  result is identical to ATAN2(Y,X). Otherwise, it is the arc tangent of X,', &
'  where the real part of the result is in radians and lies in the range -PI/2', &
'  <= ATAN(X) <= PI/2', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atan', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'      implicit none', &
'      character(len=*),parameter :: all=''(*(g0,1x))''', &
'      real(kind=real64),parameter :: &', &
'       Deg_Per_Rad = 57.2957795130823208767981548_real64', &
'      real(kind=real64) :: x', &
'          x=2.866_real64', &
'          print all, atan(x)', &
'', &
'          print all, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad', &
'          print all, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad', &
'          print all, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad', &
'          print all, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad', &
'', &
'      end program demo_atan', &
'', &
'  Results:', &
'', &
'         1.235085437457879', &
'         .7853981633974483 45.00000000000000', &
'         2.356194490192345 135.0000000000000', &
'         -.7853981633974483 -45.00000000000000', &
'         -2.356194490192345 -135.0000000000000', &
'', &
'STANDARD', &
'  FORTRAN 77 for a complex argument; and for two arguments Fortran 2008', &
'', &
'SEE ALSO', &
'  ATAN2(3), TAN(3)', &
'', &
'RESOURCES', &
'  o  wikipedia: inverse trigonometric functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                atan(3fortran)', &
'']

shortname="atan"
call process()


case('18','atanh')

textblock=[character(len=256) :: &
'', &
'atanh(3fortran)                                                atanh(3fortran)', &
'', &
'NAME', &
'  ATANH(3) - [MATHEMATICS:TRIGONOMETRIC] Inverse hyperbolic tangent function', &
'', &
'SYNOPSIS', &
'  result = atanh(x)', &
'', &
'           elemental TYPE(kind=KIND) function atanh(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be real or complex of any associated type', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  ATANH(3) computes the inverse hyperbolic tangent of X.', &
'', &
'OPTIONS', &
'  o  X : The type shall be real or complex.', &
'', &
'RESULT', &
'  The return value has same type and kind as X. If X is complex, the imaginary', &
'  part of the result is in radians and lies between', &
'', &
'             **-PI/2 <= aimag(atanh(x)) <= PI/2**', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atanh', &
'      implicit none', &
'      real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]', &
'', &
'         write (*,*) atanh(x)', &
'', &
'      end program demo_atanh', &
'', &
'  Results:', &
'', &
'       >       -Infinity  0.0000000E+00       Infinity', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  Inverse function: TANH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:hyperbolic functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               atanh(3fortran)', &
'']

shortname="atanh"
call process()


case('19','atomic_add')

textblock=[character(len=256) :: &
'', &
'atomic_add(3fortran)                                      atomic_add(3fortran)', &
'', &
'NAME', &
'  ATOMIC_ADD(3) - [ATOMIC] Atomic ADD operation', &
'', &
'SYNOPSIS', &
'  call atomic_add (atom, value [,stat] )', &
'', &
'           subroutine atomic_add(atom,value,stat)', &
'', &
'            integer(atomic_int_kind)            :: atom[*]', &
'            integer(atomic_int_kind),intent(in) :: value', &
'            integer,intent(out),intent(out)     :: stat', &
'', &
'CHARACTERISTICS', &
'  o  ATOM is a scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE is a scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT is a Scalar default-kind integer variable.', &
'', &
'DESCRIPTION', &
'  ATOMIC_ADD(3) atomically adds the value of VAR to the variable ATOM.  When', &
'  STAT is present and the invocation was successful, it is assigned the value', &
'  0. If it is present and the invocation has failed, it is assigned a positive', &
'  value; in particular, for a coindexed ATOM, if the remote image has stopped,', &
'  it is assigned the value of iso_fortran_env''s STAT_STOPPED_IMAGE and if the', &
'  remote image has failed, the value STAT_FAILED_IMAGE.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_add', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*]', &
'         call atomic_add (atom[1], this_image())', &
'      end program demo_atomic_add', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_FETCH_ADD(3), ATOMIC_AND(3), ATOMIC_OR(3),', &
'  ATOMIC_XOR(3) ISO_FORTRAN_ENV(3),', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          atomic_add(3fortran)', &
'']

shortname="atomic_add"
call process()


case('20','atomic_and')

textblock=[character(len=256) :: &
'', &
'atomic_and(3fortran)                                      atomic_and(3fortran)', &
'', &
'NAME', &
'  ATOMIC_AND(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise AND operation', &
'', &
'SYNOPSIS', &
'  call atomic_and(atom, value [,stat])', &
'', &
'           subroutine atomic_and(atom,value,stat)', &
'', &
'            integer(atomic_int_kind)            :: atom[*]', &
'            integer(atomic_int_kind),intent(in) :: value', &
'            integer,intent(out),intent(out)     :: stat', &
'', &
'CHARACTERISTICS', &
'  o  ATOM is a scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE is a scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT is a Scalar default-kind integer variable.', &
'', &
'DESCRIPTION', &
'  ATOMIC_AND(3) atomically defines ATOM with the bitwise AND between the', &
'  values of ATOM and VALUE. When STAT is present and the invocation was', &
'  successful, it is assigned the value 0. If it is present and the invocation', &
'  has failed, it is assigned a positive value; in particular, for a coindexed', &
'  ATOM, if the remote image has stopped, it is assigned the value of', &
'  iso_fortran_env''s stat_stopped_image and if the remote image has failed, the', &
'  value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_and', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*]', &
'         call atomic_and(atom[1], int(b''10100011101''))', &
'      end program demo_atomic_and', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_FETCH_AND(3), ATOMIC_DEFINE(3), ATOMIC_REF(3), ATOMIC_CAS(3),', &
'  ISO_FORTRAN_ENV(3), ATOMIC_ADD(3), ATOMIC_OR(3), ATOMIC_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          atomic_and(3fortran)', &
'']

shortname="atomic_and"
call process()


case('21','atomic_cas')

textblock=[character(len=256) :: &
'', &
'atomic_cas(3fortran)                                      atomic_cas(3fortran)', &
'', &
'NAME', &
'  ATOMIC_CAS(3) - [ATOMIC] Atomic compare and swap', &
'', &
'SYNOPSIS', &
'  call atomic_cas (atom, old, compare, new [,stat] )', &
'', &
'           subroutine atomic_cas (atom, old, compare, new, stat)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  ATOMIC_CAS(3) compares the variable ATOM with the value of COMPARE; if the', &
'  value is the same, ATOM is set to the value of NEW. Additionally, OLD is set', &
'  to the value of ATOM that was used for the comparison. When STAT is present', &
'  and the invocation was successful, it is assigned the value 0. If it is', &
'  present and the invocation has failed, it is assigned a positive value; in', &
'  particular, for a coindexed ATOM, if the remote image has stopped, it is', &
'  assigned the value of iso_fortran_env''s stat_stopped_image and if the remote', &
'  image has failed, the value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of either integer type with', &
'     atomic_int_kind kind or logical type with atomic_logical_kind kind.', &
'', &
'  o  OLD : Scalar of the same type and kind as ATOM.', &
'', &
'  o  COMPARE : Scalar variable of the same type and kind as ATOM.', &
'', &
'  o  NEW : Scalar variable of the same type as ATOM. If kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_cas', &
'      use iso_fortran_env', &
'      implicit none', &
'      logical(atomic_logical_kind) :: atom[*], prev', &
'         call atomic_cas(atom[1], prev, .false., .true.)', &
'      end program demo_atomic_cas', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_REF(3), ISO_FORTRAN_ENV(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          atomic_cas(3fortran)', &
'']

shortname="atomic_cas"
call process()


case('22','atomic_define')

textblock=[character(len=256) :: &
'', &
'atomic_define(3fortran)                                atomic_define(3fortran)', &
'', &
'NAME', &
'  ATOMIC_DEFINE(3) - [ATOMIC] Setting a variable atomically', &
'', &
'SYNOPSIS', &
'  call atomic_define (atom, value [,stat] )', &
'', &
'           subroutine atomic_define(atom, value, stat)', &
'', &
'            TYPE(kind=atomic_KIND_kind) :: atom[*]', &
'            TYPE(kind=KIND) :: value', &
'            integer,intent(out),optional :: stat', &
'', &
'CHARACTERISTICS', &
'  o  ATOM : Scalar coarray or coindexed variable of either integer type with', &
'     atomic_int_kind kind or logical type with atomic_logical_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'DESCRIPTION', &
'  ATOMIC_DEFINE(3) defines the variable ATOM with the value VALUE atomically.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable to atomically assign the', &
'     value VALUE to. kind.', &
'', &
'  o  VALUE : value to assign to ATOM', &
'', &
'  o  STAT : When STAT is present and the invocation was successful, it is', &
'     assigned the value 0. If it is present and the invocation has failed, it', &
'     is assigned a positive value; in particular, for a coindexed ATOM, if the', &
'     remote image has stopped, it is assigned the value of iso_fortran_env''s', &
'     stat_stopped_image and if the remote image has failed, the value', &
'     stat_failed_image.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_define', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*]', &
'          call atomic_define(atom[1], this_image())', &
'      end program demo_atomic_define', &
'', &
'STANDARD', &
'  Fortran 2008 ; with STAT, TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_REF(3), ATOMIC_CAS(3), ISO_FORTRAN_ENV(3), ATOMIC_ADD(3),', &
'  ATOMIC_AND(3), ATOMIC_OR(3), ATOMIC_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022       atomic_define(3fortran)', &
'']

shortname="atomic_define"
call process()


case('23','atomic_fetch_add')

textblock=[character(len=256) :: &
'', &
'atomic_fetch_add(3fortran)                          atomic_fetch_add(3fortran)', &
'', &
'NAME', &
'  ATOMIC_FETCH_ADD(3) - [ATOMIC] Atomic ADD operation with prior fetch', &
'', &
'SYNOPSIS', &
'  call atomic_fetch_add(atom, value, old [,stat] )', &
'', &
'           subroutine atomic_fetch_add(atom, value, old, stat)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  ATOMIC_FETCH_ADD(3) atomically stores the value of ATOM in OLD and adds the', &
'  value of VAR to the variable ATOM. When STAT is present and the invocation', &
'  was successful, it is assigned the value 0. If it is present and the', &
'  invocation has failed, it is assigned a positive value; in particular, for a', &
'  coindexed ATOM, if the remote image has stopped, it is assigned the value of', &
'  iso_fortran_env''s stat_stopped_image and if the remote image has failed, the', &
'  value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind. atomic_logical_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  OLD : Scalar of the same type and kind as ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_fetch_add', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*], old', &
'         call atomic_add(atom[1], this_image(), old)', &
'      end program demo_atomic_fetch_add', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_ADD(3), ISO_FORTRAN_ENV(3),', &
'', &
'  ATOMIC_FETCH_AND(3), ATOMIC_FETCH_OR(3),', &
'', &
'  ATOMIC_FETCH_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022    atomic_fetch_add(3fortran)', &
'']

shortname="atomic_fetch_add"
call process()


case('24','atomic_fetch_and')

textblock=[character(len=256) :: &
'', &
'atomic_fetch_and(3fortran)                          atomic_fetch_and(3fortran)', &
'', &
'NAME', &
'  ATOMIC_FETCH_AND(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise AND operation', &
'  with prior fetch', &
'', &
'SYNOPSIS', &
'  call atomic_fetch_and(atom, value, old [,stat] )', &
'', &
'           subroutine atomic_fetch_and(atom, value, old, stat)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  ATOMIC_FETCH_AND(3) atomically stores the value of ATOM in OLD and defines', &
'  ATOM with the bitwise AND between the values of ATOM and VALUE.  When STAT', &
'  is present and the invocation was successful, it is assigned the value 0. If', &
'  it is present and the invocation has failed, it is assigned a positive', &
'  value; in particular, for a coindexed ATOM, if the remote image has stopped,', &
'  it is assigned the value of iso_fortran_env''s stat_stopped_image and if the', &
'  remote image has failed, the value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  OLD : Scalar of the same type and kind as ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_fetch_and', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*], old', &
'         call atomic_fetch_and (atom[1], int(b''10100011101''), old)', &
'      end program demo_atomic_fetch_and', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_AND(3), ISO_FORTRAN_ENV(3),', &
'', &
'  ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_OR(3),', &
'', &
'  ATOMIC_FETCH_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022    atomic_fetch_and(3fortran)', &
'']

shortname="atomic_fetch_and"
call process()


case('25','atomic_fetch_or')

textblock=[character(len=256) :: &
'', &
'atomic_fetch_or(3fortran)                            atomic_fetch_or(3fortran)', &
'', &
'NAME', &
'  ATOMIC_FETCH_OR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise OR operation', &
'  with prior fetch', &
'', &
'SYNOPSIS', &
'  call atomic_fetch_or(atom, value, old [,stat] )', &
'', &
'           subroutine atomic_fetch_or(atom, value, old, stat)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  ATOMIC_FETCH_OR(3) atomically stores the value of ATOM in OLD and defines', &
'  ATOM with the bitwise OR between the values of ATOM and VALUE.  When STAT is', &
'  present and the invocation was successful, it is assigned the value 0. If it', &
'  is present and the invocation has failed, it is assigned a positive value;', &
'  in particular, for a coindexed ATOM, if the remote image has stopped, it is', &
'  assigned the value of iso_fortran_env''s stat_stopped_image and if the remote', &
'  image has failed, the value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  OLD : Scalar of the same type and kind as ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_fetch_or', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*], old', &
'         call atomic_fetch_or(atom[1], int(b''10100011101''), old)', &
'      end program demo_atomic_fetch_or', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_OR(3), ISO_FORTRAN_ENV(3),', &
'', &
'  ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_AND(3),', &
'', &
'  ATOMIC_FETCH_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022     atomic_fetch_or(3fortran)', &
'']

shortname="atomic_fetch_or"
call process()


case('26','atomic_fetch_xor')

textblock=[character(len=256) :: &
'', &
'atomic_fetch_xor(3fortran)                          atomic_fetch_xor(3fortran)', &
'', &
'NAME', &
'  ATOMIC_FETCH_XOR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise XOR operation', &
'  with prior fetch', &
'', &
'SYNOPSIS', &
'  call atomic_fetch_xor (atom, value, old [,stat] )', &
'', &
'           subroutine atomic_fetch_xor (atom, value, old, stat)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  ATOMIC_FETCH_XOR(3) atomically stores the value of ATOM in OLD and defines', &
'  ATOM with the bitwise XOR between the values of ATOM and VALUE.  When STAT', &
'  is present and the invocation was successful, it is assigned the value 0. If', &
'  it is present and the invocation has failed, it is assigned a positive', &
'  value; in particular, for a coindexed ATOM, if the remote image has stopped,', &
'  it is assigned the value of iso_fortran_env''s stat_stopped_image and if the', &
'  remote image has failed, the value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  OLD : Scalar of the same type and kind as ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_fetch_xor', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*], old', &
'         call atomic_fetch_xor (atom[1], int(b''10100011101''), old)', &
'      end program demo_atomic_fetch_xor', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_XOR(3), ISO_FORTRAN_ENV(3),', &
'', &
'  ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_AND(3),', &
'', &
'  ATOMIC_FETCH_OR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022    atomic_fetch_xor(3fortran)', &
'']

shortname="atomic_fetch_xor"
call process()


case('27','atomic_or')

textblock=[character(len=256) :: &
'', &
'atomic_or(3fortran)                                        atomic_or(3fortran)', &
'', &
'NAME', &
'  ATOMIC_OR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise OR operation', &
'', &
'SYNOPSIS', &
'  call atomic_or(atom, value [,stat] )', &
'', &
'           subroutine atomic_or(atom,value,stat)', &
'', &
'            integer(atomic_int_kind)            :: atom[*]', &
'            integer(atomic_int_kind),intent(in) :: value', &
'            integer,intent(out),intent(out)     :: stat', &
'', &
'CHARACTERISTICS', &
'  o  ATOM is a scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE is a scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT is a Scalar default-kind integer variable.', &
'', &
'DESCRIPTION', &
'  ATOMIC_OR(3) atomically defines ATOM with the bitwise OR between the values', &
'  of ATOM and VALUE. When STAT is present and the invocation was successful,', &
'  it is assigned the value 0. If it is present and the invocation has failed,', &
'  it is assigned a positive value; in particular, for a coindexed ATOM, if the', &
'  remote image has stopped, it is assigned the value of iso_fortran_env''s', &
'  stat_stopped_image and if the remote image has failed, the value', &
'  stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_or', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*]', &
'         call atomic_or(atom[1], int(b''10100011101''))', &
'      end program demo_atomic_or', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_FETCH_OR(3),', &
'', &
'  ISO_FORTRAN_ENV(3), ATOMIC_ADD(3), ATOMIC_OR(3),', &
'', &
'  ATOMIC_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           atomic_or(3fortran)', &
'']

shortname="atomic_or"
call process()


case('28','atomic_ref')

textblock=[character(len=256) :: &
'', &
'atomic_ref(3fortran)                                      atomic_ref(3fortran)', &
'', &
'NAME', &
'  ATOMIC_REF(3) - [ATOMIC] Obtaining the value of a variable atomically', &
'', &
'SYNOPSIS', &
'  call atomic_ref(value, atom [,stat] )', &
'', &
'           subroutine atomic_ref(value,atom,stat)', &
'', &
'            integer(atomic_int_kind),intent(in) :: value', &
'            integer(atomic_int_kind)            :: atom[*]', &
'            integer,intent(out),intent(out)     :: stat', &
'', &
'CHARACTERISTICS', &
'  o  ATOM is a scalar coarray or coindexed variable of either integer type', &
'     with atomic_int_kind kind or logical type with atomic_logical_kind kind.', &
'', &
'  o  VALUE is a scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT is a Scalar default-kind integer variable.', &
'', &
'DESCRIPTION', &
'  ATOMIC_REF(3) atomically assigns the value of the variable ATOM to VALUE.', &
'  When STAT is present and the invocation was successful, it is assigned the', &
'  value 0. If it is present and the invocation has failed, it is assigned a', &
'  positive value; in particular, for a coindexed ATOM, if the remote image has', &
'  stopped, it is assigned the value of iso_fortran_env''s STAT_STOPPED_IMAGE', &
'  and if the remote image has failed, the value STAT_FAILED_IMAGE.', &
'', &
'OPTIONS', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  ATOM : Scalar coarray or coindexed variable of either integer type with', &
'     atomic_int_kind kind or logical type with atomic_logical_kind kind.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_ref', &
'      use iso_fortran_env', &
'      implicit none', &
'      logical(atomic_logical_kind) :: atom[*]', &
'      logical :: val', &
'         call atomic_ref( val, atom[1] )', &
'         if (val) then', &
'            print *, "Obtained"', &
'         endif', &
'      end program demo_atomic_ref', &
'', &
'STANDARD', &
'  Fortran 2008 ; with STAT, TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_CAS(3), ISO_FORTRAN_ENV(3),', &
'', &
'  ATOMIC_FETCH_ADD(3), ATOMIC_FETCH_AND(3),', &
'', &
'  ATOMIC_FETCH_OR(3), ATOMIC_FETCH_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          atomic_ref(3fortran)', &
'']

shortname="atomic_ref"
call process()


case('29','atomic_xor')

textblock=[character(len=256) :: &
'', &
'atomic_xor(3fortran)                                      atomic_xor(3fortran)', &
'', &
'NAME', &
'  ATOMIC_XOR(3) - [ATOMIC:BIT MANIPULATION] Atomic bitwise OR operation', &
'', &
'SYNOPSIS', &
'  call atomic_xor(atom, value [,stat] )', &
'', &
'           subroutine atomic_xor(atom,value,stat)', &
'', &
'            integer(atomic_int_kind)            :: atom[*]', &
'            integer(atomic_int_kind),intent(in) :: value', &
'            integer,intent(out),intent(out)     :: stat', &
'', &
'CHARACTERISTICS', &
'  o  ATOM is a scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE is a scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT is a Scalar default-kind integer variable.', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  ATOMIC_XOR(3) atomically defines ATOM with the bitwise XOR between the', &
'  values of ATOM and VALUE. When STAT is present and the invocation was', &
'  successful, it is assigned the value 0. If it is present and the invocation', &
'  has failed, it is assigned a positive value; in particular, for a coindexed', &
'  ATOM, if the remote image has stopped, it is assigned the value of', &
'  iso_fortran_env''s stat_stopped_image and if the remote image has failed, the', &
'  value stat_failed_image.', &
'', &
'OPTIONS', &
'  o  ATOM : Scalar coarray or coindexed variable of integer type with', &
'     atomic_int_kind kind.', &
'', &
'  o  VALUE : Scalar of the same type as ATOM. If the kind is different, the', &
'     value is converted to the kind of ATOM.', &
'', &
'  o  STAT : (optional) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_atomic_xor', &
'      use iso_fortran_env', &
'      implicit none', &
'      integer(atomic_int_kind) :: atom[*]', &
'         call atomic_xor(atom[1], int(b''10100011101''))', &
'      end program demo_atomic_xor', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ATOMIC_DEFINE(3), ATOMIC_FETCH_XOR(3), ISO_FORTRAN_ENV(3), ATOMIC_ADD(3),', &
'  ATOMIC_OR(3), ATOMIC_XOR(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          atomic_xor(3fortran)', &
'']

shortname="atomic_xor"
call process()


case('30','bessel_j0')

textblock=[character(len=256) :: &
'', &
'bessel_j0(3fortran)                                        bessel_j0(3fortran)', &
'', &
'NAME', &
'  BESSEL_J0(3) - [MATHEMATICS] Bessel function of the first kind of order 0', &
'', &
'SYNOPSIS', &
'  result = bessel_j0(x)', &
'', &
'           elemental real(kind=KIND) function bessel_j0(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  KIND may be any KIND supported by the real type.', &
'', &
'  o  The result is the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  BESSEL_J0(3) computes the Bessel function of the first kind of order 0 of X.', &
'', &
'OPTIONS', &
'  o  X : The value to operate on.', &
'', &
'RESULT', &
'  the Bessel function of the first kind of order 0 of X. The result lies in', &
'  the range -0.4027 <= BESSEL(0,X) <= 1.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bessel_j0', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'      & real32, real64, real128', &
'         implicit none', &
'         real(kind=real64) :: x', &
'         x = 0.0_real64', &
'         x = bessel_j0(x)', &
'         write(*,*)x', &
'      end program demo_bessel_j0', &
'', &
'  Results:', &
'', &
'            1.0000000000000000', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           bessel_j0(3fortran)', &
'']

shortname="bessel_j0"
call process()


case('31','bessel_j1')

textblock=[character(len=256) :: &
'', &
'bessel_j1(3fortran)                                        bessel_j1(3fortran)', &
'', &
'NAME', &
'  BESSEL_J1(3) - [MATHEMATICS] Bessel function of the first kind of order 1', &
'', &
'SYNOPSIS', &
'  result = bessel_j1(x)', &
'', &
'           elemental real(kind=KIND) function bessel_j1(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  KIND may be any supported real KIND.', &
'', &
'  o  the result is of the same type and kind as X', &
'', &
'DESCRIPTION', &
'  BESSEL_J1(3) computes the Bessel function of the first kind of order 1 of X.', &
'', &
'OPTIONS', &
'  o  X : The type shall be real.', &
'', &
'RESULT', &
'  The return value is of type real and lies in the range -0.5818 <=', &
'  BESSEL(0,X) <= 0.5818 . It has the same kind as X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bessel_j1', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 1.0_real64', &
'         x = bessel_j1(x)', &
'         write(*,*)x', &
'      end program demo_bessel_j1', &
'', &
'  Results:', &
'', &
'           0.44005058574493350', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BESSEL_J0(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           bessel_j1(3fortran)', &
'']

shortname="bessel_j1"
call process()


case('32','bessel_jn')

textblock=[character(len=256) :: &
'', &
'bessel_jn(3fortran)                                        bessel_jn(3fortran)', &
'', &
'NAME', &
'  BESSEL_JN(3) - [MATHEMATICS] Bessel function of the first kind', &
'', &
'SYNOPSIS', &
'  result = bessel_jn(n, x)', &
'', &
'           elemental real(kind=KIND) function bessel_jn(n,x)', &
'', &
'            integer(kind=**),intent(in) :: n', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'  o  KIND may be any valid value for type real', &
'', &
'     o  X is real', &
'', &
'     o  The return value has the same type and kind as X.', &
'', &
'             result = bessel_jn(n1, n2, x)', &
'', &
'              real(kind=KIND) function bessel_jn(n1, n2, ,x)', &
'', &
'              integer(kind=**),intent(in) :: n1', &
'              integer(kind=**),intent(in) :: n2', &
'              real(kind=KIND),intent(in) :: x', &
'', &
'     o  N1 is integer', &
'', &
'     o  N2 is integer', &
'', &
'     o  X is real', &
'', &
'     o  The return value has the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  BESSEL_JN( N, X ) computes the Bessel function of the first kind of order N', &
'  of X.', &
'', &
'  BESSEL_JN(N1, N2, X) returns an array with the Bessel function|Bessel', &
'  functions of the first kind of the orders N1 to N2.', &
'', &
'OPTIONS', &
'  o  N : a non-negative scalar integer..', &
'', &
'  o  N1 : a non-negative scalar integer.', &
'', &
'  o  N2 : a non-negative scalar integer.', &
'', &
'  o  X : Shall be a scalar for BESSEL_JN(N,X) or an array For BESSEL_JN(N1,', &
'     N2, X).', &
'', &
'RESULT', &
'  The result value of BESSEL_JN (N, X) is a processor-dependent approximation', &
'  to the Bessel function of the first kind and order N of X.', &
'', &
'  The result of BESSEL_JN (N1, N2, X) is a rank-one array with extent MAX', &
'  (N2-N1+1, 0). Element i of the result value of BESSEL_JN (N1, N2, X) is a', &
'  processor-dependent approximation to the Bessel function of the first kind', &
'  and order N1+i-1 of X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bessel_jn', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'         & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 1.0_real64', &
'          x = bessel_jn(5,x)', &
'          write(*,*)x', &
'      end program demo_bessel_jn', &
'', &
'  Results:', &
'', &
'            2.4975773021123450E-004', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BESSEL_J0(3), BESSEL_J1(3), BESSEL_Y0(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           bessel_jn(3fortran)', &
'']

shortname="bessel_jn"
call process()


case('33','bessel_y0')

textblock=[character(len=256) :: &
'', &
'bessel_y0(3fortran)                                        bessel_y0(3fortran)', &
'', &
'NAME', &
'  BESSEL_Y0(3) - [MATHEMATICS] Bessel function of the second kind of order 0', &
'', &
'SYNOPSIS', &
'  result = bessel_y0(x)', &
'', &
'           elemental real(kind=KIND) function bessel_y0(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  KIND may be any supported real KIND.', &
'', &
'  o  the result characteristics (type, kind) are the same as X', &
'', &
'DESCRIPTION', &
'  BESSEL_Y0(3) computes the Bessel function of the second kind of order 0 of', &
'  X.', &
'', &
'OPTIONS', &
'  o  X : The type shall be real. Its value shall be greater than zero.', &
'', &
'RESULT', &
'  The return value is of type real. It has the same kind as X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bessel_y0', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'      & real32, real64, real128', &
'      implicit none', &
'        real(kind=real64) :: x = 0.0_real64', &
'        x = bessel_y0(x)', &
'        write(*,*)x', &
'      end program demo_bessel_y0', &
'', &
'  Results:', &
'', &
'                          -Infinity', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BESSEL_J0(3), BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y1(3), BESSEL_YN(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           bessel_y0(3fortran)', &
'']

shortname="bessel_y0"
call process()


case('34','bessel_y1')

textblock=[character(len=256) :: &
'', &
'bessel_y1(3fortran)                                        bessel_y1(3fortran)', &
'', &
'NAME', &
'  BESSEL_Y1(3) - [MATHEMATICS] Bessel function of the second kind of order 1', &
'', &
'SYNOPSIS', &
'  result = bessel_y1(x)', &
'', &
'           elemental real(kind=KIND) function bessel_y1(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  KIND may be any supported real KIND.', &
'', &
'  o  the characteristics (type, kind) of the result are the same as X', &
'', &
'DESCRIPTION', &
'  BESSEL_Y1(3) computes the Bessel function of the second kind of order 1 of', &
'  X.', &
'', &
'OPTIONS', &
'  o  X : The type shall be real. Its value shall be greater than zero.', &
'', &
'RESULT', &
'  The return value is real. It has the same kind as X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bessel_y1', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'      & real32, real64, real128', &
'      implicit none', &
'        real(kind=real64) :: x = 1.0_real64', &
'        write(*,*)x, bessel_y1(x)', &
'      end program demo_bessel_y1', &
'', &
'  Results:', &
'', &
'       >    1.00000000000000      -0.781212821300289', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BESSEL_J0(3), BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_YN(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           bessel_y1(3fortran)', &
'']

shortname="bessel_y1"
call process()


case('35','bessel_yn')

textblock=[character(len=256) :: &
'', &
'bessel_yn(3fortran)                                        bessel_yn(3fortran)', &
'', &
'NAME', &
'  BESSEL_YN(3) - [MATHEMATICS] Bessel function of the second kind', &
'', &
'SYNOPSIS', &
'  result = bessel_yn(n, x)', &
'', &
'           elemental real(kind=KIND) function bessel_yn(n,x)', &
'', &
'            integer(kind=**),intent(in) :: n', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  N is integer', &
'', &
'  o  X is real', &
'', &
'  o  The return value has the same type and kind as X.', &
'', &
'          result = bessel_yn(n1, n2, x)', &
'', &
'           real(kind=KIND) function bessel_yn(n1, n2, ,x)', &
'', &
'            integer(kind=**),intent(in) :: n1', &
'            integer(kind=**),intent(in) :: n2', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'  o  N1 is integer', &
'', &
'  o  N2 is integer', &
'', &
'  o  X is real', &
'', &
'  o  The return value has the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  BESSEL_YN(N, X) computes the Bessel function of the second kind of order N', &
'  of X.', &
'', &
'  BESSEL_YN(N1, N2, X) returns an array with the Bessel function|Bessel', &
'  functions of the first kind of the orders N1 to N2.', &
'', &
'OPTIONS', &
'  o  N : Shall be a scalar or an array of type integer and non-negative.', &
'', &
'  o  N1 : Shall be a non-negative scalar of type integer and non-negative.', &
'', &
'  o  N2 : Shall be a non-negative scalar of type integer and non-negative.', &
'', &
'  o  X : A real non-negative value. Note BESSEL_YN(N1, N2, X) is not', &
'     elemental, in which case it must be a scalar.', &
'', &
'RESULT', &
'  The result value of BESSEL_YN (N, X) is a processor-dependent approximation', &
'  to the Bessel function of the second kind and order N of X.', &
'', &
'  The result of BESSEL_YN (N1, N2, X) is a rank-one array with extent MAX', &
'  (N2-N1+1, 0). Element i of the result value of BESSEL_YN (N1, N2, X) is a', &
'  processor-dependent approximation to the Bessel function of the second kind', &
'  and order N1+i-1 of X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bessel_yn', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'      & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 1.0_real64', &
'        write(*,*) x,bessel_yn(5,x)', &
'      end program demo_bessel_yn', &
'', &
'  Results:', &
'', &
'            1.0000000000000000       -260.40586662581222', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BESSEL_J0(3), BESSEL_J1(3), BESSEL_JN(3), BESSEL_Y0(3), BESSEL_Y1(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           bessel_yn(3fortran)', &
'']

shortname="bessel_yn"
call process()


case('36','bge')

textblock=[character(len=256) :: &
'', &
'bge(3fortran)                                                    bge(3fortran)', &
'', &
'NAME', &
'  BGE(3) - [BIT:COMPARE] Bitwise greater than or equal to', &
'', &
'SYNOPSIS', &
'  result = bge(i,j)', &
'', &
'            elemental logical function bge(i, j)', &
'', &
'             integer(kind=**),intent(in) :: i', &
'             integer(kind=**),intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  the integer kind of I and J may not necessarily be the same. In addition,', &
'     values may be a BOZ constant with a value valid for the integer kind', &
'     available with the most bits on the current platform.', &
'', &
'  o  The return value is of type default logical.', &
'', &
'DESCRIPTION', &
'  BGE(3) Determines whether one integer is bitwise greater than or equal to', &
'  another.', &
'', &
'  The bit-level representation of a value is platform dependent. The endian-', &
'  ness of a system and whether the system uses a "two''s complement"', &
'  representation of signs can affect the results, for example.', &
'', &
'  A BOZ constant (Binary, Octal, Hexadecimal) does not have a kind or type of', &
'  its own, so be aware it is subject to truncation when transferred to an', &
'  integer type. The most bits the constant may contain is limited by the most', &
'  bits representable by any integer kind supported by the compilation.', &
'', &
'  Bit Sequence Comparison', &
'', &
'  When bit sequences of unequal length are compared, the shorter sequence is', &
'  padded with zero bits on the left to the same length as the longer sequence', &
'  (up to the largest number of bits any available integer kind supports).', &
'', &
'  Bit sequences are compared from left to right, one bit at a time, until', &
'  unequal bits are found or until all bits have been compared and found to be', &
'  equal.', &
'', &
'  The bits are always evaluated in this order, not necessarily from MSB to LSB', &
'  (most significant bit to least significant bit).', &
'', &
'  If unequal bits are found the sequence with zero in the unequal position is', &
'  considered to be less than the sequence with one in the unequal position.', &
'', &
'OPTIONS', &
'  o  I : The value to test if >= J based on the bit representation of the', &
'     values.', &
'', &
'  o  J : The value to test I against.', &
'', &
'RESULT', &
'  Returns .true. if I is bit-wise greater than J and .false. otherwise.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bge', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer            :: i', &
'      integer(kind=int8) :: byte', &
'      integer(kind=int8),allocatable :: arr1(:), arr2(:)', &
'', &
'        ! BASIC USAGE', &
'         write(*,*)''bge(-127,127)='',bge( -127, 127 )', &
'         ! on (very common) "two''s complement" machines that are', &
'         ! little-endian -127 will be greater than 127', &
'', &
'         ! BOZ constants', &
'         ! BOZ constants are subject to truncation, so make sure', &
'         ! your values are valid for the integer kind being compared to', &
'         write(*,*)''bge(b"0001",2)='',bge( b"1", 2)', &
'', &
'        ! ELEMENTAL', &
'         ! an array and scalar', &
'         write(*, *)''compare array of values [-128, -0, +0, 127] to 127''', &
'         write(*, *)bge(int([-128, -0, +0, 127], kind=int8), 127_int8)', &
'', &
'         ! two arrays', &
'         write(*, *)''compare two arrays''', &
'         arr1=int( [ -127, -0, +0,  127], kind=int8 )', &
'         arr2=int( [  127,  0,  0, -127], kind=int8 )', &
'         write(*,*)''arr1='',arr1', &
'         write(*,*)''arr2='',arr2', &
'         write(*, *)''bge(arr1,arr2)='',bge( arr1, arr2 )', &
'', &
'        ! SHOW TESTS AND BITS', &
'         ! actually looking at the bit patterns should clarify what affect', &
'         ! signs have ...', &
'         write(*,*)''Compare some one-byte values to 64.''', &
'         write(*,*)''Notice that the values are tested as bits not as integers''', &
'         write(*,*)''so the results are as if values are unsigned integers.''', &
'         do i=-128,127,32', &
'            byte=i', &
'            write(*,''(sp,i0.4,*(1x,1l,1x,b0.8))'')i,bge(byte,64_int8),byte', &
'         enddo', &
'', &
'        ! SIGNED ZERO', &
'         ! are +0 and -0 the same on your platform? When comparing at the', &
'         ! bit level this is important', &
'         write(*,''("plus zero=",b0)'')  +0', &
'         write(*,''("minus zero=",b0)'') -0', &
'', &
'      end program demo_bge', &
'', &
'  Results:', &
'', &
'  How an integer value is represented at the bit level can vary. These are', &
'  just the values expected on Today''s most common platforms ...', &
'', &
'          > bge(-127,127)= T', &
'          > bge(b"0001",2)= F', &
'          > compare array of values [-128, -0, +0, 127] to 127', &
'          > T F F T', &
'          > compare two arrays', &
'          > arr1= -127    0    0  127', &
'          > arr2=  127    0    0 -127', &
'          > bge(arr1,arr2)= T T T F', &
'          > Compare some one-byte values to 64.', &
'          > Notice that the values are tested as bits not as integers', &
'          > so the results are as if values are unsigned integers.', &
'          > -0128  T 10000000', &
'          > -0096  T 10100000', &
'          > -0064  T 11000000', &
'          > -0032  T 11100000', &
'          > +0000  F 00000000', &
'          > +0032  F 00100000', &
'          > +0064  T 01000000', &
'          > +0096  T 01100000', &
'          > plus zero=0', &
'          > minus zero=0', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BGT(3), BLE(3), BLT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 bge(3fortran)', &
'']

shortname="bge"
call process()


case('37','bgt')

textblock=[character(len=256) :: &
'', &
'bgt(3fortran)                                                    bgt(3fortran)', &
'', &
'NAME', &
'  BGT(3) - [BIT:COMPARE] Bitwise greater than', &
'', &
'SYNOPSIS', &
'  result = bgt(i, j)', &
'', &
'            elemental logical function bgt(i, j)', &
'', &
'             integer(kind=**),intent(in) :: i', &
'             integer(kind=**),intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  I is an integer or a boz-literal-constant.', &
'', &
'  o  J is an integer or a boz-literal-constant.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type The', &
'     integer kind of I and J may not necessarily be the same. kind. In', &
'     addition, values may be a BOZ constant with a value valid for the integer', &
'     kind available with the most bits on the current platform.', &
'', &
'  o  The return value is of type logical and of the default kind.', &
'', &
'DESCRIPTION', &
'  BGT determines whether an integer is bitwise greater than another.  Bit-', &
'  level representations of values are platform-dependent.', &
'', &
'OPTIONS', &
'  o  I : reference value to compare against', &
'', &
'  o  J : value to compare to I', &
'', &
'RESULT', &
'  The return value is of type logical and of the default kind. The result is', &
'  true if the sequence of bits represented by i is greater than the sequence', &
'  of bits represented by j, otherwise the result is false.', &
'', &
'  Bits are compared from right to left.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bgt', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer            :: i', &
'      integer(kind=int8) :: byte', &
'        ! Compare some one-byte values to 64.', &
'         ! Notice that the values are tested as bits not as integers', &
'         ! so sign bits in the integer are treated just like any other', &
'         write(*,''(a)'') ''we will compare other values to 64''', &
'         i=64', &
'         byte=i', &
'         write(*,''(sp,i0.4,*(1x,1l,1x,b0.8))'')i,bgt(byte,64_int8),byte', &
'', &
'         write(*,''(a)'') "comparing at the bit level, not as whole numbers."', &
'         write(*,''(a)'') "so pay particular attention to the negative"', &
'         write(*,''(a)'') "values on this two''s complement platform ..."', &
'         do i=-128,127,32', &
'            byte=i', &
'            write(*,''(sp,i0.4,*(1x,1l,1x,b0.8))'')i,bgt(byte,64_int8),byte', &
'         enddo', &
'', &
'         ! see the BGE() description for an extended description', &
'         ! of related information', &
'', &
'      end program demo_bgt', &
'', &
'  Results:', &
'', &
'       > we will compare other values to 64', &
'       > +0064  F 01000000', &
'       > comparing at the bit level, not as whole numbers.', &
'       > so pay particular attention to the negative', &
'       > values on this two''s complement platform ...', &
'       > -0128  T 10000000', &
'       > -0096  T 10100000', &
'       > -0064  T 11000000', &
'       > -0032  T 11100000', &
'       > +0000  F 00000000', &
'       > +0032  F 00100000', &
'       > +0064  F 01000000', &
'       > +0096  T 01100000', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BGE(3), BLE(3), BLT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 bgt(3fortran)', &
'']

shortname="bgt"
call process()


case('38','bit_size')

textblock=[character(len=256) :: &
'', &
'bit_size(3fortran)                                          bit_size(3fortran)', &
'', &
'NAME', &
'  BIT_SIZE(3) - [BIT:INQUIRY] Bit size inquiry function', &
'', &
'SYNOPSIS', &
'  result = bit_size(i)', &
'', &
'           integer(kind=KIND) function bit_size(i)', &
'', &
'            integer(kind=KIND),intent(in) :: i(..)', &
'', &
'CHARACTERISTICS', &
'  o  I shall be of type integer. It may be a scalar or an array.', &
'', &
'  o  the value of KIND is any valid value for an integer kind parameter on the', &
'     processor.', &
'', &
'  o  the return value is a scalar of the same kind as the input value.', &
'', &
'DESCRIPTION', &
'  BIT_SIZE(3) returns the number of bits (integer precision plus sign bit)', &
'  represented by the type of the integer I.', &
'', &
'OPTIONS', &
'  o  I : An integer value of any kind whose size in bits is to be determined.', &
'     Because only the type of the argument is examined, the argument need not', &
'     be defined; I can be a scalar or an array, but a scalar representing just', &
'     a single element is always returned.', &
'', &
'RESULT', &
'  The number of bits used to represent a value of the type and kind of i.  The', &
'  result is a integer scalar of the same kind as i.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_bit_size', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      use,intrinsic :: iso_fortran_env, only : integer_kinds', &
'      implicit none', &
'      character(len=*),parameter   :: fmt=&', &
'      & ''(a,": bit size is ",i3," which is kind=",i3," on this platform")''', &
'', &
'          ! default integer bit size on this platform', &
'          write(*,fmt) "default", bit_size(0), kind(0)', &
'', &
'          write(*,fmt) "int8   ", bit_size(0_int8),   kind(0_int8)', &
'          write(*,fmt) "int16  ", bit_size(0_int16),  kind(0_int16)', &
'          write(*,fmt) "int32  ", bit_size(0_int32),  kind(0_int32)', &
'          write(*,fmt) "int64  ", bit_size(0_int64),  kind(0_int64)', &
'', &
'          write(*,''(a,*(i0:,", "))'') "The available kinds are ",integer_kinds', &
'', &
'      end program demo_bit_size', &
'', &
'  Typical Results:', &
'', &
'          default: bit size is  32 which is kind=  4 on this platform', &
'          int8   : bit size is   8 which is kind=  1 on this platform', &
'          int16  : bit size is  16 which is kind=  2 on this platform', &
'          int32  : bit size is  32 which is kind=  4 on this platform', &
'          int64  : bit size is  64 which is kind=  8 on this platform', &
'          The available kinds are 1, 2, 4, 8, 16', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022            bit_size(3fortran)', &
'']

shortname="bit_size"
call process()


case('39','ble')

textblock=[character(len=256) :: &
'', &
'ble(3fortran)                                                    ble(3fortran)', &
'', &
'NAME', &
'  BLE(3) - [BIT:COMPARE] Bitwise less than or equal to', &
'', &
'SYNOPSIS', &
'  result = ble(i,j)', &
'', &
'           elemental logical function ble(i, j)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  I and J may be of any supported integer kind, not necessarily the same.', &
'     An exception is that values may be a BOZ constant with a value valid for', &
'     the integer kind available with the most bits on the current platform.', &
'', &
'  o  the returned value is a logical scalar of default kind', &
'', &
'DESCRIPTION', &
'  BLE(3) determines whether an integer is bitwise less than or equal to', &
'  another, assuming any shorter value is padded on the left with zeros to the', &
'  length of the longer value.', &
'', &
'OPTIONS', &
'  o  I : the value to compare J to', &
'', &
'  o  J : the value to be tested for being less than or equal to I', &
'', &
'RESULT', &
'  The return value is .true. if any bit in J is less than any bit in I', &
'  starting with the rightmost bit and continuing tests leftward.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ble', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer            :: i', &
'      integer(kind=int8) :: byte', &
'        ! Compare some one-byte values to 64.', &
'         ! Notice that the values are tested as bits not as integers', &
'         ! so sign bits in the integer are treated just like any other', &
'         do i=-128,127,32', &
'            byte=i', &
'            write(*,''(sp,i0.4,*(1x,1l,1x,b0.8))'')i,ble(byte,64_int8),byte', &
'            write(*,''(sp,i0.4,*(4x,b0.8))'')64_int8,64_int8', &
'         enddo', &
'', &
'         ! see the BGE() description for an extended description', &
'         ! of related information', &
'', &
'      end program demo_ble', &
'', &
'  Results:', &
'', &
'         -0128  F 10000000', &
'         +0064    01000000', &
'         -0096  F 10100000', &
'         +0064    01000000', &
'         -0064  F 11000000', &
'         +0064    01000000', &
'         -0032  F 11100000', &
'         +0064    01000000', &
'         +0000  T 00000000', &
'         +0064    01000000', &
'         +0032  T 00100000', &
'         +0064    01000000', &
'         +0064  T 01000000', &
'         +0064    01000000', &
'         +0096  F 01100000', &
'         +0064    01000000', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BGE(3), BGT(3), BLT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 ble(3fortran)', &
'']

shortname="ble"
call process()


case('40','blt')

textblock=[character(len=256) :: &
'', &
'blt(3fortran)                                                    blt(3fortran)', &
'', &
'NAME', &
'  BLT(3) - [BIT:COMPARE] Bitwise less than', &
'', &
'SYNOPSIS', &
'  result = blt(i,j)', &
'', &
'           elemental logical function blt(i, j)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  I is an integer of any kind or a BOZ-literal-constant', &
'', &
'  o  J is an integer of any kind or a BOZ-literal-constant, not necessarily', &
'     the same as I.', &
'', &
'  o  the result is of default logical kind', &
'', &
'  BOZ constants must have a value valid for the integer kind available with', &
'  the most bits on the current platform.', &
'', &
'DESCRIPTION', &
'  BLT(3) determines whether an integer is bitwise less than another.', &
'', &
'OPTIONS', &
'  o  I : Shall be of integer type or a BOZ literal constant.', &
'', &
'  o  J : Shall be of integer type or a BOZ constant.', &
'', &
'RESULT', &
'  The return value is of type logical and of the default kind.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_blt', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer            :: i', &
'      integer(kind=int8) :: byte', &
'        ! Compare some one-byte values to 64.', &
'         ! Notice that the values are tested as bits not as integers', &
'         ! so sign bits in the integer are treated just like any other', &
'         do i=-128,127,32', &
'            byte=i', &
'            write(*,''(sp,i0.4,*(1x,1l,1x,b0.8))'')i,blt(byte,64_int8),byte', &
'         enddo', &
'        ! BOZ literals', &
'         write(*,*)blt(z''1000'', z''101011010'')', &
'         ! see the BGE() description for an extended description', &
'         ! of related information', &
'', &
'      end program demo_blt', &
'', &
'  Results:', &
'', &
'         > -0128  F 10000000', &
'         > -0096  F 10100000', &
'         > -0064  F 11000000', &
'         > -0032  F 11100000', &
'         > +0000  T 00000000', &
'         > +0032  T 00100000', &
'         > +0064  F 01000000', &
'         > +0096  F 01100000', &
'         > T', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BGE(3), BGT(3), BLE(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 blt(3fortran)', &
'']

shortname="blt"
call process()


case('41','btest')

textblock=[character(len=256) :: &
'', &
'btest(3fortran)                                                btest(3fortran)', &
'', &
'NAME', &
'  BTEST(3) - [BIT:INQUIRY] Tests a bit of an integer value.', &
'', &
'SYNOPSIS', &
'  result = btest(i,pos)', &
'', &
'           elemental logical function btest(i,pos)', &
'', &
'            integer(kind=**),intent(in)  :: i', &
'            integer(kind=**),intent(in)  :: pos', &
'', &
'CHARACTERISTICS', &
'  o  I is an integer of any kind', &
'', &
'  o  POS is a integer of any kind', &
'', &
'  o  the result is a default logical', &
'', &
'DESCRIPTION', &
'  BTEST(3) returns logical .true. if the bit at POS in I is set to 1.', &
'  Position zero is the right-most bit. Bit position increases from right to', &
'  left up to BITSIZE(I)-1.', &
'', &
'OPTIONS', &
'  o  I : The integer containing the bit to be tested', &
'', &
'  o  POS : The position of the bit to query. it must be a valid position for', &
'     the value I; ie. 0 <= POS <= BIT_SIZE(I).', &
'', &
'RESULT', &
'  The result is a logical that has the value .true. if bit position POS of I', &
'  has the value 1 and the value .false. if bit POS of I has the value 0.', &
'', &
'  Positions of bits in the sequence are numbered from right to left, with the', &
'  position of the rightmost bit being zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_btest', &
'      implicit none', &
'      integer :: i, j, pos, a(2,2)', &
'      logical :: bool', &
'      character(len=*),parameter :: g=''(*(g0))''', &
'', &
'           i = 32768 + 1024 + 64', &
'          write(*,''(a,i0,"=>",b32.32,/)'')''Looking at the integer: '',i', &
'', &
'          ! looking one bit at a time from LOW BIT TO HIGH BIT', &
'          write(*,g)''from bit 0 to bit '',bit_size(i),''==>''', &
'          do pos=0,bit_size(i)-1', &
'              bool = btest(i, pos)', &
'              write(*,''(l1)'',advance=''no'')bool', &
'          enddo', &
'          write(*,*)', &
'', &
'          ! a binary format the hard way.', &
'          ! Note going from bit_size(i) to zero.', &
'          write(*,*)', &
'          write(*,g)''so for '',i,'' with a bit size of '',bit_size(i)', &
'          write(*,''(b32.32)'')i', &
'          write(*,g)merge(''^'',''_'',[(btest(i,j),j=bit_size(i)-1,0,-1)])', &
'          write(*,*)', &
'          write(*,g)''and for '',-i,'' with a bit size of '',bit_size(i)', &
'          write(*,''(b32.32)'')-i', &
'          write(*,g)merge(''^'',''_'',[(btest(-i,j),j=bit_size(i)-1,0,-1)])', &
'', &
'          ! elemental:', &
'          !', &
'          a(1,:)=[ 1, 2 ]', &
'          a(2,:)=[ 3, 4 ]', &
'          write(*,*)', &
'          write(*,''(a,/,*(i2,1x,i2,/))'')''given the array a ...'',a', &
'          ! the second bit of all the values in a', &
'          write(*,''(a,/,*(l2,1x,l2,/))'')''the value of btest (a, 2)'',btest(a,2)', &
'          ! bits 1,2,3,4 of the value 2', &
'          write(*,''(a,/,*(l2,1x,l2,/))'')''the value of btest (2, a)'',btest(2,a)', &
'      end program demo_btest', &
'', &
'  Results:', &
'', &
'        > Looking at the integer: 33856=>11111111111111110111101111000000', &
'        >', &
'        > 00000000000000001000010001000000', &
'        > 11111111111111110111101111000000', &
'        > 1000010001000000', &
'        > 11111111111111110111101111000000', &
'        > from bit 0 to bit 32==>', &
'        > FFFFFFTFFFTFFFFTFFFFFFFFFFFFFFFF', &
'        >', &
'        > so for 33856 with a bit size of 32', &
'        > 00000000000000001000010001000000', &
'        > ________________^____^___^______', &
'        >', &
'        > and for -33856 with a bit size of 32', &
'        > 11111111111111110111101111000000', &
'        > ^^^^^^^^^^^^^^^^_^^^^_^^^^______', &
'        >', &
'        > given the array a ...', &
'        >  1  3', &
'        >  2  4', &
'        >', &
'        > the value of btest (a, 2)', &
'        >  F  F', &
'        >  F  T', &
'        >', &
'        > the value of btest (2, a)', &
'        >  T  F', &
'        >  F  F', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), IBCLR(3), NOT(3), IBCLR(3), IBITS(3), IBSET(3), IAND(3), IOR(3),', &
'  IEOR(3), MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               btest(3fortran)', &
'']

shortname="btest"
call process()


case('42','c_associated')

textblock=[character(len=256) :: &
'', &
'c_associated(3fortran)                                  c_associated(3fortran)', &
'', &
'NAME', &
'  C_ASSOCIATED(3) - [ISO_C_BINDING] Status of a C pointer', &
'', &
'SYNOPSIS', &
'  result = c_associated(c_prt_1, [c_ptr_2] )', &
'', &
'           logical function c_associated(c_prt_1, cptr_2)', &
'', &
'            TYPE,intent(in) ::c_ptr_1', &
'            TYPE,intent(in),optional ::c_ptr_2', &
'', &
'CHARACTERISTICS', &
'  o  C_PTR_1 is a scalar of the type c_ptr or c_funptr.', &
'', &
'  o  C_PTR_2 is a scalar of the same type as c_ptr_1.', &
'', &
'  o  The return value is of type logical', &
'', &
'DESCRIPTION', &
'  C_ASSOCIATED(3) determines the status of the C pointer c_ptr_1 or if c_ptr_1', &
'  is associated with the target c_ptr_2.', &
'', &
'OPTIONS', &
'  o  C_PTR_1 : C pointer to test for being a C NULL pointer, or to test if', &
'     pointing to the same association as C_PTR_2 when present.', &
'', &
'  o  C_PTR_2 : C pointer to test for shared association with C_PTR_1', &
'', &
'RESULT', &
'  The return value is of type logical; it is .false. if either c_ptr_1 is a C', &
'  NULL pointer or if c_ptr1 and c_ptr_2 point to different addresses.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_c_associated', &
'', &
'      contains', &
'', &
'      subroutine association_test(a,b)', &
'      use iso_c_binding, only: c_associated, c_loc, c_ptr', &
'      implicit none', &
'      real, pointer :: a', &
'      type(c_ptr) :: b', &
'         if(c_associated(b, c_loc(a))) &', &
'            stop ''b and a do not point to same target''', &
'      end subroutine association_test', &
'', &
'      end program demo_c_associated', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  C_LOC(3), C_FUNLOC(3), ISO_C_BINDING(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022        c_associated(3fortran)', &
'']

shortname="c_associated"
call process()


case('43','ceiling')

textblock=[character(len=256) :: &
'', &
'ceiling(3fortran)                                            ceiling(3fortran)', &
'', &
'NAME', &
'  CEILING(3) - [NUMERIC] Integer ceiling function', &
'', &
'SYNOPSIS', &
'  result = ceiling(a [,kind])', &
'', &
'           elemental integer(KIND) function ceiling(a,KIND)', &
'', &
'            real(kind=**),intent(in)  :: a', &
'            integer,intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  ** a is of type real', &
'', &
'  o  KIND shall be a scalar integer constant expression. It specifies the kind', &
'     of the result if present.', &
'', &
'  o  the result is integer. It is default kind if KIND is not specified', &
'', &
'DESCRIPTION', &
'  CEILING(3) returns the least integer greater than or equal to A.', &
'', &
'  On the number line -n <-- 0 -> +n the value returned is always at or to the', &
'  right of the input value.', &
'', &
'OPTIONS', &
'  o  A : A real value to produce a ceiling for.', &
'', &
'  o  KIND : indicates the kind parameter of the result.', &
'', &
'RESULT', &
'  The result will be the integer value equal to A or the least integer greater', &
'  than A if the input value is not equal to a whole number.', &
'', &
'  If A is equal to a whole number, the returned value is INT(A).', &
'', &
'  The result is undefined if it cannot be represented in the specified integer', &
'  type.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ceiling', &
'      implicit none', &
'      ! just a convenient format for a list of integers', &
'      character(len=*),parameter :: ints=''(*("   > ",5(i0:,",",1x),/))''', &
'      real :: x', &
'      real :: y', &
'        ! basic usage', &
'         x = 63.29', &
'         y = -63.59', &
'         print ints, ceiling(x)', &
'         print ints, ceiling(y)', &
'         ! note the result was the next integer larger to the right', &
'', &
'        ! real values equal to whole numbers', &
'         x = 63.0', &
'         y = -63.0', &
'         print ints, ceiling(x)', &
'         print ints, ceiling(y)', &
'', &
'        ! elemental (so an array argument is allowed)', &
'         print ints , &', &
'         & ceiling([ &', &
'         &  -2.7,  -2.5, -2.2, -2.0, -1.5, &', &
'         &  -1.0,  -0.5,  0.0, +0.5, +1.0, &', &
'         &  +1.5,  +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'      end program demo_ceiling', &
'', &
'  Results:', &
'', &
'         > 64', &
'         > -63', &
'         > 63', &
'         > -63', &
'         > -2, -2, -2, -2, -1,', &
'         > -1, 0, 0, 1, 1,', &
'         > 2, 2, 3, 3, 3', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  FLOOR(3), NINT(3)', &
'', &
'  AINT(3), ANINT(3), INT(3), SELECTED_INT_KIND(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             ceiling(3fortran)', &
'']

shortname="ceiling"
call process()


case('44','c_f_pointer')

textblock=[character(len=256) :: &
'', &
'c_f_pointer(3fortran)                                    c_f_pointer(3fortran)', &
'', &
'NAME', &
'  C_F_POINTER(3) - [ISO_C_BINDING] Convert C into Fortran pointer', &
'', &
'SYNOPSIS', &
'  call c_f_pointer(cptr, fptr [,shape] )', &
'', &
'           subroutine c_f_pointer(cptr, fptr ,shape )', &
'', &
'            type(c_ptr),intent(in) :: cprt', &
'            type(TYPE),pointer,intent(out) :: fprt', &
'            integer,intent(in),optional :: shape(:)', &
'', &
'CHARACTERISTICS', &
'  The Fortran pointer FPRT must be interoperable with CPTR', &
'', &
'  SHAPE is only specified if FPTR is an array.', &
'', &
'DESCRIPTION', &
'  C_F_POINTER(3) assigns the target (the C pointer CPTR) to the Fortran', &
'  pointer FPTR and specifies its shape if FPTR points to an array.', &
'', &
'OPTIONS', &
'  o  CPTR : scalar of the type c_ptr. It is INTENT(IN).', &
'', &
'  o  FPTR : pointer interoperable with CPTR. it is INTENT(OUT).', &
'', &
'  o  SHAPE : (Optional) Rank-one array of type integer with INTENT(IN) .  It', &
'     shall be present if and only if FPTR is an array. The size must be equal', &
'     to the rank of FPTR.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_c_f_pointer', &
'      use iso_c_binding', &
'      implicit none', &
'      interface', &
'         subroutine my_routine(p) bind(c,name=''myC_func'')', &
'            import :: c_ptr', &
'            type(c_ptr), intent(out) :: p', &
'         end subroutine', &
'      end interface', &
'      type(c_ptr) :: cptr', &
'      real,pointer :: a(:)', &
'         call my_routine(cptr)', &
'         call c_f_pointer(cptr, a, [12])', &
'      end program demo_c_f_pointer', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  C_LOC(3), C_F_PROCPOINTER(3), ISO_C_BINDING(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022         c_f_pointer(3fortran)', &
'']

shortname="c_f_pointer"
call process()


case('45','c_f_procpointer')

textblock=[character(len=256) :: &
'', &
'c_f_procpointer(3fortran)                            c_f_procpointer(3fortran)', &
'', &
'NAME', &
'  C_F_PROCPOINTER(3) - [ISO_C_BINDING] Convert C into Fortran procedure', &
'  pointer', &
'', &
'SYNOPSIS', &
'  call c_f_procpointer(cptr, fptr)', &
'', &
'           subroutine c_f_procpointer(cptr, fptr )', &
'', &
'            type(c_funptr),intent(in) :: cprt', &
'            type(TYPE),pointer,intent(out) :: fprt', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  C_F_PROCPOINTER(3) assigns the target of the C function pointer CPTR to the', &
'  Fortran procedure pointer FPTR.', &
'', &
'OPTIONS', &
'  o  CPTR : scalar of the type c_funptr. It is INTENT(IN).', &
'', &
'  o  FPTR : procedure pointer interoperable with CPTR. It is INTENT(OUT).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_c_f_procpointer', &
'      use iso_c_binding', &
'      implicit none', &
'      abstract interface', &
'         function func(a)', &
'         import :: c_float', &
'         real(c_float), intent(in) :: a', &
'         real(c_float) :: func', &
'         end function', &
'      end interface', &
'      interface', &
'         function getIterFunc() bind(c,name="getIterFunc")', &
'         import :: c_funptr', &
'         type(c_funptr) :: getIterFunc', &
'         end function', &
'      end interface', &
'      type(c_funptr) :: cfunptr', &
'      procedure(func), pointer :: myFunc', &
'         cfunptr = getIterFunc()', &
'         call c_f_procpointer(cfunptr, myFunc)', &
'      end program demo_c_f_procpointer', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  C_LOC(3), C_F_POINTER(3), ISO_C_BINDING(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022     c_f_procpointer(3fortran)', &
'']

shortname="c_f_procpointer"
call process()


case('46','c_funloc')

textblock=[character(len=256) :: &
'', &
'c_funloc(3fortran)                                          c_funloc(3fortran)', &
'', &
'NAME', &
'  C_FUNLOC(3) - [ISO_C_BINDING] Obtain the C address of a procedure', &
'', &
'SYNOPSIS', &
'  result = c_funloc(x)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  C_FUNLOC(3) determines the C address of the argument.', &
'', &
'OPTIONS', &
'  o  X : Interoperable function or pointer to such function.', &
'', &
'RESULT', &
'  The return value is of type c_funptr and contains the C address of the', &
'  argument.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      ! program demo_c_funloc and module', &
'      module x', &
'      use iso_c_binding', &
'      implicit none', &
'      contains', &
'      subroutine sub(a) bind(c)', &
'      real(c_float) :: a', &
'         a = sqrt(a)+5.0', &
'      end subroutine sub', &
'      end module x', &
'      !', &
'      program demo_c_funloc', &
'      use iso_c_binding', &
'      use x', &
'      implicit none', &
'      interface', &
'         subroutine my_routine(p) bind(c,name=''myC_func'')', &
'           import :: c_funptr', &
'           type(c_funptr), intent(in) :: p', &
'         end subroutine', &
'      end interface', &
'         call my_routine(c_funloc(sub))', &
'      !', &
'      end program demo_c_funloc', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  C_ASSOCIATED(3), C_LOC(3), C_F_POINTER(3),', &
'', &
'  C_F_PROCPOINTER(3), ISO_C_BINDING(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022            c_funloc(3fortran)', &
'']

shortname="c_funloc"
call process()


case('47','char')

textblock=[character(len=256) :: &
'', &
'char(3fortran)                                                  char(3fortran)', &
'', &
'NAME', &
'  CHAR(3) - [CHARACTER] Generate a character from a code value', &
'', &
'SYNOPSIS', &
'  result = char(i [,kind])', &
'', &
'           elemental character(kind=KIND) function char(i,KIND)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer of any kind', &
'', &
'  o  KIND is an integer initialization expression indicating the kind', &
'     parameter of the result.', &
'', &
'  o  The returned value is a character with the kind specified by KIND or if', &
'     KIND is not present, the default character kind.', &
'', &
'DESCRIPTION', &
'  Generates a character value given a numeric code representing the position I', &
'  in the collating sequence associated with the specified kind KIND.', &
'', &
'  Note that ACHAR(3) is a similar function specifically for ASCII characters', &
'  that is preferred when only ASCII is being processed, which is equivalent to', &
'  CHAR(I,KIND=SELECTED_CHAR_KIND("ASCII") )', &
'', &
'  The ICHAR(3) function is the reverse of CHAR(3), converting characters to', &
'  their collating sequence value.', &
'', &
'OPTIONS', &
'  o  I : a value in the range 0 <= I <= N-1, where N is the number of', &
'     characters in the collating sequence associated with the specified kind', &
'     type parameter. : For ASCII, N is 127. The default character set may or', &
'     may not allow higher values.', &
'', &
'  o  KIND : A constant integer initialization expression indicating the kind', &
'     parameter of the result. If not present, the default kind is assumed.', &
'', &
'RESULT', &
'  The return value is a single character of the specified kind, determined by', &
'  the position of I in the collating sequence associated with the specified', &
'  KIND.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_char', &
'      implicit none', &
'      integer, parameter :: ascii =  selected_char_kind ("ascii")', &
'      character(len=1, kind=ascii ) :: c', &
'      integer :: i', &
'        ! basic', &
'         i=74', &
'         c=char(i)', &
'         write(*,*)''ASCII character '',i,''is '',c', &
'        !', &
'         print *, ''a selection of ASCII characters (shows hex if not printable)''', &
'         do i=0,127,10', &
'            c = char(i,kind=ascii)', &
'            select case(i)', &
'            case(32:126)', &
'               write(*,''(i3,1x,a)'')i,c', &
'            case(0:31,127)', &
'               ! print hexadecimal value for unprintable characters', &
'               write(*,''(i3,1x,z2.2)'')i,c', &
'            case default', &
'               write(*,''(i3,1x,a,1x,a)'')i,c,''non-standard ASCII''', &
'            end select', &
'         enddo', &
'', &
'      end program demo_char', &
'', &
'  Results:', &
'', &
'          ASCII character           74 is J', &
'          a selection of ASCII characters (shows hex if not printable)', &
'           0 00', &
'          10 0A', &
'          20 14', &
'          30 1E', &
'          40 (', &
'          50 2', &
'          60 <', &
'          70 F', &
'          80 P', &
'          90 Z', &
'  100 d 110 n 120 x', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ACHAR(3), IACHAR(3), ICHAR(3)', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                char(3fortran)', &
'']

shortname="char"
call process()


case('48','c_loc')

textblock=[character(len=256) :: &
'', &
'c_loc(3fortran)                                                c_loc(3fortran)', &
'', &
'NAME', &
'  C_LOC(3) - [ISO_C_BINDING] Obtain the C address of an object', &
'', &
'SYNOPSIS', &
'  result = c_loc(x)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  C_LOC(3) determines the C address of the argument.', &
'', &
'OPTIONS', &
'  o  X : Shall have either the pointer or target attribute. It shall not be a', &
'     coindexed object. It shall either be a variable with interoperable type', &
'     and kind type parameters, or be a scalar, nonpolymorphic variable with no', &
'     length type parameters.', &
'', &
'RESULT', &
'  The return value is of type c_ptr and contains the C address of the', &
'  argument.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'         subroutine association_test(a,b)', &
'         use iso_c_binding, only: c_associated, c_loc, c_ptr', &
'         implicit none', &
'         real, pointer :: a', &
'         type(c_ptr) :: b', &
'           if(c_associated(b, c_loc(a))) &', &
'              stop ''b and a do not point to same target''', &
'         end subroutine association_test', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  C_ASSOCIATED(3), C_FUNLOC(3), C_F_POINTER(3),', &
'', &
'  C_F_PROCPOINTER(3), ISO_C_BINDING(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022               c_loc(3fortran)', &
'']

shortname="c_loc"
call process()


case('49','cmplx')

textblock=[character(len=256) :: &
'', &
'cmplx(3fortran)                                                cmplx(3fortran)', &
'', &
'NAME', &
'  CMPLX(3) - [TYPE:NUMERIC] Conversion to a complex type', &
'', &
'SYNOPSIS', &
'  result = cmplx(x [,kind]) | cmplx(x [,y] [,kind])', &
'', &
'           elemental complex(kind=KIND) function cmplx( x, y, kind )', &
'', &
'            type(TYPE(kind=**)),intent(in)          :: x', &
'            type(TYPE(kind=**)),intent(in),optional :: y', &
'            integer(kind=**),intent(in),optional    :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  X may be integer, real, or complex.', &
'', &
'  o  Y may be integer or real. Y is allowed only if X is not complex.', &
'', &
'  o  KIND is a constant integer initialization expression indicating the kind', &
'     parameter of the result.', &
'', &
'  The type of the arguments does not affect the kind of the result except for', &
'  a complex X value.', &
'', &
'  o  if KIND is not present and X is complex the result is of the kind of X.', &
'', &
'  o  if KIND is not present and X is not complex the result if of default', &
'     complex kind.', &
'', &
'  NOTE: a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  The CMPLX(3) function converts numeric values to a complex value.', &
'', &
'  Even though constants can be used to define a complex variable using syntax', &
'  like', &
'', &
'            z = (1.23456789, 9.87654321)', &
'', &
'  this will not work for variables. So you cannot enter', &
'', &
'            z = (a, b)  ! NO ! (unless a and b are constants, not variables)', &
'', &
'  so to construct a complex value using non-complex values you must use the', &
'  CMPLX(3) function:', &
'', &
'            z = cmplx(a, b)', &
'', &
'  or assign values separately to the imaginary and real components using the', &
'  %IM and %RE designators:', &
'', &
'            z%re = a', &
'            z%im = b', &
'', &
'  If X is complex Y is not allowed and CMPLX essentially returns the input', &
'  value except for an optional change of kind, which can be useful when', &
'  passing a value to a procedure that requires the arguments to have a', &
'  different kind (and does not return an altered value):', &
'', &
'            call something(cmplx(z,kind=real64))', &
'', &
'  would pass a copy of a value with kind=real64 even if z had a different kind', &
'', &
'  but otherwise is equivalent to a simple assign. So if z1 and z2 were', &
'  complex:', &
'', &
'            z2 = z1        ! equivalent statements', &
'            z2 = cmplx(z1)', &
'', &
'  If X is not complex X is only used to define the real component of the', &
'  result but Y is still optional -- the imaginary part of the result will just', &
'  be assigned a value of zero.', &
'', &
'  If Y is present it is converted to the imaginary component.', &
'', &
'  CMPLX(3) AND DOUBLE PRECISION', &
'', &
'  Primarily in order to maintain upward compatibility you need to be careful', &
'  when working with complex values of higher precision that the default.', &
'', &
'  It was necessary for Fortran to continue to specify that CMPLX(3) always', &
'  return a result of the default kind if the KIND option is absent, since that', &
'  is the behavior mandated by FORTRAN 77.', &
'', &
'  It might have been preferable to use the highest precision of the arguments', &
'  for determining the return kind, but that is not the case. So with arguments', &
'  with greater precision than default values you are required to use the KIND', &
'  argument or the greater precision values will be reduced to default', &
'  precision.', &
'', &
'  This means CMPLX(D1,D2), where D1 and D2 are doubleprecision, is treated as:', &
'', &
'            cmplx(sngl(d1), sngl(d2))', &
'', &
'  which looses precision.', &
'', &
'  So Fortran 90 extends the CMPLX(3) intrinsic by adding an extra argument', &
'  used to specify the desired kind of the complex result.', &
'', &
'            integer,parameter :: dp=kind(0.0d0)', &
'            complex(kind=dp) :: z8', &
'  ! wrong ways to specify constant values ! note this was stored with default', &
'  real precision !  z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)', &
'  print *, ''NO, Z8='',z8,real(z8),aimag(z8)', &
'', &
'    z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp) ! again, note', &
'    output components are just real print *, ''NO, Z8='',z8,real(z8),aimag(z8) !', &
'    ! YES !  ! kind= makes it work z8 = cmplx(1.2345678901234567d0,', &
'    1.2345678901234567d0,kind=dp) print *, ''YES, Z8='',z8,real(z8),aimag(z8)', &
'', &
'  A more recent alternative to using CMPLX(3) is "F2018 component syntax"', &
'  where real and imaginary parts of a complex entity can be accessed', &
'  independently:', &
'', &
'      value%RE     ! %RE specifies the real part', &
'      or', &
'      value%IM     ! %IM specifies the imaginary part', &
'', &
'  Where the designator value is of course of complex type.', &
'', &
'  The type of a complex-part-designator is real, and its kind and shape are', &
'  those of the designator. That is, you retain the precision of the complex', &
'  value by default, unlike with CMPLX.', &
'', &
'  The following are examples of complex part designators:', &
'', &
'             impedance%re           !-- Same value as real(impedance)', &
'             fft%im                 !-- Same value as AIMAG(fft)', &
'             x%im = 0.0             !-- Sets the imaginary part of x to zero', &
'             x(1:2)%re=[10,20]      !-- even if x is an array', &
'', &
'  NOTE for I/O', &
'', &
'  Note that if format statements are specified a complex value is treated as', &
'  two real values.', &
'', &
'  For list-directed I/O (ie. using an asterisk for a format) and NAMELIST', &
'  output the values are expected to be delimited by "(" and ")" and of the', &
'  form "(real_part,imaginary_part)". For NAMELIST input parenthesized values', &
'  or lists of multiple real values are acceptable.', &
'', &
'OPTIONS', &
'  o  X : The value assigned to the real component of the result when X is not', &
'     complex.', &
'', &
'     If X is complex, the result is the same as if the real part of the input', &
'     was passed as X and the imaginary part as Y.', &
'', &
'              result = CMPLX (REAL (X), AIMAG (X), KIND).', &
'', &
'     That is, a complex X value is copied to the result value with a possible', &
'     change of kind.', &
'', &
'  o  Y : Y is only allowed if X is not complex. Its value is assigned to the', &
'     imaginary component of the result and defaults to a value of zero if', &
'     absent.', &
'', &
'  o  KIND : An integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'RESULT', &
'  The return value is of complex type, with magnitudes determined by the', &
'  values X and Y.', &
'', &
'  The common case when X is not complex is that the real component of the', &
'  result is assigned the value of X and the imaginary part is zero or the', &
'  value of Y if Y is present.', &
'', &
'  When X is complex Y is not allowed and the result is the same value as X', &
'  with a possible change of kind. That is, the real part is REAL(X, KIND) and', &
'  the imaginary part is REAL(Y, KIND).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_aimag', &
'      implicit none', &
'      integer,parameter :: dp=kind(0.0d0)', &
'      real(kind=dp)     :: precise', &
'      complex(kind=dp)  :: z8', &
'      complex           :: z4, zthree(3)', &
'         precise=1.2345678901234567d0', &
'', &
'        ! basic', &
'         z4 = cmplx(-3)', &
'         print *, ''Z4='',z4', &
'         z4 = cmplx(1.23456789, 1.23456789)', &
'         print *, ''Z4='',z4', &
'         ! with a format treat a complex as two real values', &
'         print ''(1x,g0,1x,g0,1x,g0)'',''Z4='',z4', &
'', &
'        ! working with higher precision values', &
'         ! using kind=dp makes it keep DOUBLEPRECISION precision', &
'         ! otherwise the result would be of default kind', &
'         z8 = cmplx(precise, -precise )', &
'         print *, ''lost precision Z8='',z8', &
'         z8 = cmplx(precise, -precise ,kind=dp)', &
'         print *, ''kept precision Z8='',z8', &
'', &
'        ! assignment of constant values does not require cmplx(3)00', &
'         ! The following is intuitive and works without calling cmplx(3)', &
'         ! but does not work for variables just constants', &
'         z8 = (1.1111111111111111d0, 2.2222222222222222d0 )', &
'         print *, ''Z8 defined with constants='',z8', &
'', &
'        ! what happens when you assign a complex to a real?', &
'         precise=z8', &
'         print *, ''LHS='',precise,''RHS='',z8', &
'', &
'        ! elemental', &
'         zthree=cmplx([10,20,30],-1)', &
'         print *, ''zthree='',zthree', &
'', &
'        ! descriptors are an alternative', &
'         zthree(1:2)%re=[100,200]', &
'         print *, ''zthree='',zthree', &
'', &
'      end program demo_aimag', &
'', &
'  Results:', &
'', &
'          Z4= (-3.000000,0.0000000E+00)', &
'          Z4= (1.234568,1.234568)', &
'          Z4= 1.234568 1.234568', &
'          lost precision Z8= (1.23456788063049,-1.23456788063049)', &
'          kept precision Z8= (1.23456789012346,-1.23456789012346)', &
'          Z8 defined with constants= (1.11111111111111,2.22222222222222)', &
'          LHS=   1.11111111111111      RHS= (1.11111111111111,2.22222222222222)', &
'          zthree= (10.00000,-1.000000) (20.00000,-1.000000) (30.00000,-1.000000)', &
'          zthree= (100.0000,-1.000000) (200.0000,-1.000000) (30.00000,-1.000000)', &
'', &
'STANDARD', &
'  FORTRAN 77, KIND added in Fortran 90.', &
'', &
'SEE ALSO', &
'  o  AIMAG(3) - Imaginary part of complex number', &
'', &
'  o  CONJG(3) - Complex conjugate function', &
'', &
'  o  REAL(3) - Convert to real type', &
'', &
'  Fortran has strong support for complex values, including many intrinsics', &
'  that take or produce complex values in addition to algebraic and logical', &
'  expressions:', &
'', &
'  ABS(3), ACOSH(3), ACOS(3), ASINH(3), ASIN(3), ATAN2(3), ATANH(3), ATAN(3),', &
'  COSH(3), COS(3), CO_SUM(3), DBLE(3), DOT_PRODUCT(3), EXP(3), INT(3),', &
'  IS_CONTIGUOUS(3), KIND(3), LOG(3), MATMUL(3), PRECISION(3), PRODUCT(3),', &
'  RANGE(3), RANK(3), SINH(3), SIN(3), SQRT(3), STORAGE_SIZE(3), SUM(3),', &
'  TANH(3), TAN(3), UNPACK(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               cmplx(3fortran)', &
'']

shortname="cmplx"
call process()


case('50','co_broadcast')

textblock=[character(len=256) :: &
'', &
'co_broadcast(3fortran)                                  co_broadcast(3fortran)', &
'', &
'NAME', &
'  CO_BROADCAST(3) - [COLLECTIVE] Copy a value to all images the current set of', &
'  images', &
'', &
'SYNOPSIS', &
'  call co_broadcast(a, source_image [,stat] [,errmsg] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_BROADCAST(3) copies the value of argument A on the image with image index', &
'  source_image to all images in the current team. A becomes defined as if by', &
'  intrinsic assignment. If the execution was successful and STAT is present,', &
'  it is assigned the value zero. If the execution failed, STAT gets assigned a', &
'  nonzero value and, if present, ERRMSG gets assigned a value describing the', &
'  occurred error.', &
'', &
'OPTIONS', &
'  o  A : INTENT(INOUT) argument; shall have the same dynamic type and type', &
'     parameters on all images of the current team. If it is an array, it shall', &
'     have the same shape on all images.', &
'', &
'  o  SOURCE_IMAGE : a scalar integer expression. It shall have the same the', &
'     same value on all images and refer to an image of the current team.', &
'', &
'  o  STAT : (optional) a scalar integer variable', &
'', &
'  o  ERRMSG : (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_co_broadcast', &
'      implicit none', &
'      integer :: val(3)', &
'         if (this_image() == 1) then', &
'            val = [1, 5, 3]', &
'         endif', &
'         call co_broadcast (val, source_image=1)', &
'         print *, this_image(), ":", val', &
'      end program demo_co_broadcast', &
'', &
'STANDARD', &
'  Fortran xx', &
'', &
'SEE ALSO', &
'  CO_MAX(3), CO_MIN(3), CO_SUM(3), CO_REDUCE(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022        co_broadcast(3fortran)', &
'']

shortname="co_broadcast"
call process()


case('51','co_lbound')

textblock=[character(len=256) :: &
'', &
'co_lbound(3fortran)                                        co_lbound(3fortran)', &
'', &
'NAME', &
'  CO_LBOUND(3) - [COLLECTIVE] Lower codimension bounds of an array', &
'', &
'SYNOPSIS', &
'  result = co_lbound( coarray [,dim] [,kind] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_LBOUND(3) returns the lower bounds of a coarray, or a single lower', &
'  cobound along the DIM codimension.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an coarray, of any type.', &
'', &
'  o  DIM : (Optional) Shall be a scalar integer.', &
'', &
'  o  KIND : (Optional) An integer initialization expression indicating the', &
'     kind parameter of the result.', &
'', &
'RESULT', &
'  The return value is of type integer and of kind KIND. If KIND is absent, the', &
'  return value is of default integer kind. If DIM is absent, the result is an', &
'  array of the lower cobounds of COARRAY. If DIM is present, the result is a', &
'  scalar corresponding to the lower cobound of the array along that', &
'  codimension.', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  CO_UBOUND(3), LBOUND(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           co_lbound(3fortran)', &
'']

shortname="co_lbound"
call process()


case('52','co_max')

textblock=[character(len=256) :: &
'', &
'co_max(3fortran)                                              co_max(3fortran)', &
'', &
'NAME', &
'  CO_MAX(3) - [COLLECTIVE] Maximal value on the current set of images', &
'', &
'SYNOPSIS', &
'  call co_max(a, result_image [,stat] [,errmsg] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_MAX(3) determines element-wise the maximal value of A on all images of', &
'  the current team. If result_image is present, the maximum values are', &
'  returned in A on the specified image only and the value of A on the other', &
'  images become undefined. If result_image is not present, the value is', &
'  returned on all images. If the execution was successful and STAT is present,', &
'  it is assigned the value zero. If the execution failed, STAT gets assigned a', &
'  nonzero value and, if present, ERRMSG gets assigned a value describing the', &
'  occurred error.', &
'', &
'OPTIONS', &
'  o  A : shall be an integer, real or character variable, which has the same', &
'     type and type parameters on all images of the team.', &
'', &
'  o  RESULT_IMAGE : (optional) a scalar integer expression; if present, it', &
'     shall have the same the same value on all images and refer to an image of', &
'     the current team.', &
'', &
'  o  STAT : (optional) a scalar integer variable', &
'', &
'  o  ERRMSG : (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_co_max', &
'      implicit none', &
'      integer :: val', &
'         val = this_image()', &
'         call co_max(val, result_image=1)', &
'         if (this_image() == 1) then', &
'           write(*,*) "Maximal value", val  ! prints num_images()', &
'         endif', &
'      end program demo_co_max', &
'', &
'  Results:', &
'', &
'          Maximal value           2', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  CO_MIN(3), CO_SUM(3), CO_REDUCE(3), CO_BROADCAST(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              co_max(3fortran)', &
'']

shortname="co_max"
call process()


case('53','co_min')

textblock=[character(len=256) :: &
'', &
'co_min(3fortran)                                              co_min(3fortran)', &
'', &
'NAME', &
'  CO_MIN(3) - [COLLECTIVE] Minimal value on the current set of images', &
'', &
'SYNOPSIS', &
'  call co_min(a, result_image [,stat] [,errmsg] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_MIN(3) determines element-wise the minimal value of A on all images of', &
'  the current team. If result_image is present, the minimal values are', &
'  returned in A on the specified image only and the value of A on the other', &
'  images become undefined. If result_image is not present, the value is', &
'  returned on all images. If the execution was successful and STAT is present,', &
'  it is assigned the value zero. If the execution failed, STAT gets assigned a', &
'  nonzero value and, if present, ERRMSG gets assigned a value describing the', &
'  occurred error.', &
'', &
'OPTIONS', &
'  o  A : shall be an integer, real or character variable, which has the same', &
'     type and type parameters on all images of the team.', &
'', &
'  o  RESULT_IMAGE : (optional) a scalar integer expression; if present, it', &
'     shall have the same the same value on all images and refer to an image of', &
'     the current team.', &
'', &
'  o  STAT : (optional) a scalar integer variable', &
'', &
'  o  ERRMSG : (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_co_min', &
'      implicit none', &
'      integer :: val', &
'         val = this_image()', &
'         call co_min(val, result_image=1)', &
'         if (this_image() == 1) then', &
'           write(*,*) "Minimal value", val  ! prints 1', &
'         endif', &
'      end program demo_co_min', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  CO_MAX(3), CO_SUM(3), CO_REDUCE(3), CO_BROADCAST(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              co_min(3fortran)', &
'']

shortname="co_min"
call process()


case('54','command_argument_count')

textblock=[character(len=256) :: &
'', &
'command_argument_count(3fortran)              command_argument_count(3fortran)', &
'', &
'NAME', &
'  COMMAND_ARGUMENT_COUNT(3) - [SYSTEM:COMMAND LINE] Get number of command line', &
'  arguments', &
'', &
'SYNOPSIS', &
'  result = command_argument_count()', &
'', &
'           integer function command_argument_count()', &
'', &
'CHARACTERISTICS', &
'  o  the result is of default integer scalar.', &
'', &
'DESCRIPTION', &
'  COMMAND_ARGUMENT_COUNT(3) returns the number of arguments passed on the', &
'  command line when the containing program was invoked.', &
'', &
'OPTIONS', &
'  None', &
'', &
'RESULT', &
'  : The return value is of type default integer. It is the number of arguments', &
'  passed on the command line when the program was invoked.', &
'', &
'  If there are no command arguments available or if the processor does not', &
'  support command arguments, then the result has the value zero.', &
'', &
'  If the processor has a concept of a command name, the command name does not', &
'  count as one of the command arguments.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_command_argument_count', &
'      implicit none', &
'      integer :: count', &
'         count = command_argument_count()', &
'         print *, count', &
'      end program demo_command_argument_count', &
'', &
'  Sample output:', &
'', &
'         # the command verb does not count', &
'         ./test_command_argument_count', &
'             0', &
'         # quoted strings may count as one argument', &
'         ./test_command_argument_count count arguments', &
'             2', &
'         ./test_command_argument_count ''count arguments''', &
'             1', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  GET_COMMAND(3), GET_COMMAND_ARGUMENT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 20command_argument_count(3fortran)', &
'']

shortname="command_argument_count"
call process()


case('55','compiler_options')

textblock=[character(len=256) :: &
'', &
'compiler_options(3fortran)                          compiler_options(3fortran)', &
'', &
'NAME', &
'  COMPILER_OPTIONS(3) - [COMPILER:INQUIRY] Options passed to the compiler', &
'', &
'SYNOPSIS', &
'  result = compiler_options()', &
'', &
'           character(len=:) function compiler_options()', &
'', &
'CHARACTERISTICS', &
'  o  the return value is a default-kind character variable with system-', &
'     dependent length.', &
'', &
'DESCRIPTION', &
'  COMPILER_OPTIONS(3) returns a string with the options used for compiling.', &
'', &
'OPTIONS', &
'  None.', &
'', &
'RESULT', &
'  The result contains the compiler flags used to compile the file containing', &
'  the COMPILER_OPTIONS(3) call.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_compiler_version', &
'      use, intrinsic :: iso_fortran_env, only : compiler_version', &
'      use, intrinsic :: iso_fortran_env, only : compiler_options', &
'      implicit none', &
'         print ''(4a)'', &', &
'            ''This file was compiled by '', &', &
'            compiler_version(),           &', &
'            '' using the options '',        &', &
'            compiler_options()', &
'      end program demo_compiler_version', &
'', &
'  Results:', &
'', &
'      This file was compiled by GCC version 10.3.0 using', &
'      the options -I build/gfortran_2A42023B310FA28D', &
'      -mtune=generic -march=x86-64 -auxbase-strip', &
'      build/gfortran_2A42023B310FA28D/compiler_options/app_main.f90.o', &
'      -g -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1', &
'      -fcheck=bounds -fcheck=array-temps -fbacktrace', &
'      -fcoarray=single -J build/gfortran_2A42023B310FA28D', &
'      -fpre-include=/usr/include/finclude/math-vector-fortran.h', &
'', &
'      This file was compiled by nvfortran 21.5-0 LLVM', &
'      using the options app/main.f90 -c -Minform=inform', &
'      -Mbackslash -Mbounds -Mchkptr -Mchkstk -traceback -module', &
'      build/nvfortran_78229DCE997517A4 -Ibuild/nvfortran_78229DCE997517A4 -o', &
'      build/nvfortran_78229DCE997517A4/compiler_options/app_main.f90.o', &
'', &
'      This file was compiled by Intel(R) Fortran Intel(R) 64 Compiler Classic', &
'      for applications running on Intel(R) 64, Version 2021.3.0 Build', &
'      20210609_000000 using the options -Ibuild/ifort_5C58216731706F11', &
'      -c -warn all -check all -error-limit 1 -O0 -g -assume', &
'      byterecl -traceback -module build/ifort_5C58216731706F11 -o', &
'      build/ifort_5C58216731706F11/compiler_options/app_main.f90.o', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  COMPILER_VERSION(3), ISO_FORTRAN_ENV(7)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022    compiler_options(3fortran)', &
'']

shortname="compiler_options"
call process()


case('56','compiler_version')

textblock=[character(len=256) :: &
'', &
'compiler_version(3fortran)                          compiler_version(3fortran)', &
'', &
'NAME', &
'  COMPILER_VERSION(3) - [COMPILER:INQUIRY] Compiler version string', &
'', &
'SYNOPSIS', &
'  result = compiler_version()', &
'', &
'           character(len=:) function compiler_version()', &
'', &
'CHARACTERISTICS', &
'  o  The return value is a default-kind scalar character with system-dependent', &
'     length.', &
'', &
'DESCRIPTION', &
'  COMPILER_VERSION(3) returns a string containing the name and version of the', &
'  compiler.', &
'', &
'OPTIONS', &
'  None.', &
'', &
'RESULT', &
'  The return value contains the name of the compiler and its version number', &
'  used to compile the file containing the COMPILER_VERSION(3) call.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_compiler_version', &
'      use, intrinsic :: iso_fortran_env, only : compiler_version', &
'      implicit none', &
'         print ''(2a)'', &', &
'            ''This file was compiled by '', &', &
'            compiler_version()', &
'      end program demo_compiler_version', &
'', &
'  Results:', &
'', &
'      This file was compiled by GCC version 10.3.0', &
'', &
'      This file was compiled by Intel(R) Fortran Intel(R) 64 Compiler', &
'      Classic for applications running on Intel(R) 64, Version 2021.3.0 Build', &
'      20210609_000000', &
'', &
'      This file was compiled by nvfortran 21.5-0 LLVM', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  COMPILER_OPTIONS(3), ISO_FORTRAN_ENV(7)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022    compiler_version(3fortran)', &
'']

shortname="compiler_version"
call process()


case('57','conjg')

textblock=[character(len=256) :: &
'', &
'conjg(3fortran)                                                conjg(3fortran)', &
'', &
'NAME', &
'  CONJG(3) - [NUMERIC] Complex conjugate of a complex value', &
'', &
'SYNOPSIS', &
'  result = conjg(z)', &
'', &
'           elemental complex(kind=KIND) function conjg(z)', &
'', &
'            complex(kind=**),intent(in) :: z', &
'', &
'CHARACTERISTICS', &
'  o  Z is a complex value of any valid kind.', &
'', &
'  o  The returned value has the same complex type as the input.', &
'', &
'DESCRIPTION', &
'  CONJG(3) returns the complex conjugate of the complex value Z.', &
'', &
'  That is, If Z is the complex value (X, Y) then the result is (X, -Y).', &
'', &
'  In mathematics, the complex conjugate of a complex number is a value whose', &
'  real and imaginary part are equal parts are equal in magnitude to each other', &
'  but the Y value has opposite sign.', &
'', &
'  For matrices of complex numbers, CONJG(ARRAY) represents the element-by-', &
'  element conjugation of ARRAY; not the conjugate transpose of the ARRAY .', &
'', &
'OPTIONS', &
'  o  Z : The value to create the conjugate of.', &
'', &
'RESULT', &
'  Returns a value equal to the input value except the sign of the imaginary', &
'  component is the opposite of the input value.', &
'', &
'  That is, if Z has the value (X,Y), the result has the value (X, -Y).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_conjg', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'      & real32, real64, real128', &
'      implicit none', &
'      complex :: z = (2.0, 3.0)', &
'      complex(kind=real64) :: dz = (   &', &
'         &  1.2345678901234567_real64, -1.2345678901234567_real64)', &
'      complex :: arr(3,3)', &
'      integer :: i', &
'         ! basics', &
'          ! notice the sine of the imaginary component changes', &
'          print *, z, conjg(z)', &
'', &
'          ! any complex kind is supported. z is of default kind but', &
'          ! dz is kind=real64.', &
'          print *, dz', &
'          dz = conjg(dz)', &
'          print *, dz', &
'          print *', &
'', &
'          ! the function is elemental so it can take arrays', &
'          arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]', &
'          arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]', &
'          arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]', &
'', &
'          write(*,*)''original''', &
'          write(*,''(3("(",g8.2,",",g8.2,")",1x))'')(arr(i,:),i=1,3)', &
'          arr = conjg(arr)', &
'          write(*,*)''conjugate''', &
'          write(*,''(3("(",g8.2,",",g8.2,")",1x))'')(arr(i,:),i=1,3)', &
'', &
'      end program demo_conjg', &
'', &
'  Results:', &
'', &
'       >  (2.000000,3.000000) (2.000000,-3.000000)', &
'       >', &
'       >  (1.23456789012346,-1.23456789012346)', &
'       >  (1.23456789012346,1.23456789012346)', &
'       >', &
'       >  original', &
'       > (-1.0    , 2.0    ) ( 3.0    , 4.0    ) ( 5.0    ,-6.0    )', &
'       > ( 7.0    ,-8.0    ) ( 8.0    , 9.0    ) ( 9.0    , 9.0    )', &
'       > ( 1.0    , 9.0    ) ( 2.0    , 0.0    ) (-3.0    ,-7.0    )', &
'       >', &
'       >  conjugate', &
'       > (-1.0    ,-2.0    ) ( 3.0    ,-4.0    ) ( 5.0    , 6.0    )', &
'       > ( 7.0    , 8.0    ) ( 8.0    ,-9.0    ) ( 9.0    ,-9.0    )', &
'       > ( 1.0    ,-9.0    ) ( 2.0    , 0.0    ) (-3.0    , 7.0    )', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  AIMAG(3) - Imaginary part of complex number', &
'', &
'  o  CMPLX(3) - Complex conversion function', &
'', &
'  o  REAL(3) - Convert to real type', &
'', &
'  Fortran has strong support for complex values, including many intrinsics', &
'  that take or produce complex values in addition to algebraic and logical', &
'  expressions:', &
'', &
'  ABS(3), ACOSH(3), ACOS(3), ASINH(3), ASIN(3), ATAN2(3), ATANH(3), ATAN(3),', &
'  COSH(3), COS(3), CO_SUM(3), DBLE(3), DOT_PRODUCT(3), EXP(3), INT(3),', &
'  IS_CONTIGUOUS(3), KIND(3), LOG(3), MATMUL(3), PRECISION(3), PRODUCT(3),', &
'  RANGE(3), RANK(3), SINH(3), SIN(3), SQRT(3), STORAGE_SIZE(3), SUM(3),', &
'  TANH(3), TAN(3), UNPACK(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               conjg(3fortran)', &
'']

shortname="conjg"
call process()


case('58','co_reduce')

textblock=[character(len=256) :: &
'', &
'co_reduce(3fortran)                                        co_reduce(3fortran)', &
'', &
'NAME', &
'  CO_REDUCE(3) - [COLLECTIVE] Reduction of values on the current set of images', &
'', &
'SYNOPSIS', &
'  call co_reduce(a, operation, result_image [,stat] [,errmsg] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_REDUCE(3) determines element-wise the reduction of the value of A on all', &
'  images of the current team. The pure function passed as OPERATION is used to', &
'  pairwise reduce the values of A by passing either the value of A of', &
'  different images or the result values of such a reduction as argument. If A', &
'  is an array, the reduction is done element wise. If result_image is present,', &
'  the result values are returned in A on the specified image only and the', &
'  value of A on the other images become undefined. If result_image is not', &
'  present, the value is returned on all images. If the execution was', &
'  successful and STAT is present, it is assigned the value zero. If the', &
'  execution failed, STAT gets assigned a nonzero value and, if present, ERRMSG', &
'  gets assigned a value describing the occurred error.', &
'', &
'OPTIONS', &
'  o  A : is an INTENT(INOUT) argument and shall be nonpolymorphic. If it is', &
'     allocatable, it shall be allocated; if it is a pointer, it shall be', &
'     associated. A shall have the same type and type parameters on all images', &
'     of the team; if it is an array, it shall have the same shape on all', &
'     images.', &
'', &
'  o  OPERATION : pure function with two scalar nonallocatable arguments, which', &
'     shall be nonpolymorphic and have the same type and type parameters as A.', &
'     The function shall return a nonallocatable scalar of the same type and', &
'     type parameters as A. The function shall be the same on all images and', &
'     with regards to the arguments mathematically commutative and associative.', &
'     Note that OPERATION may not be an elemental unless it is an intrinsic', &
'     function.', &
'', &
'  o  RESULT_IMAGE', &
'', &
'      : (optional) a scalar integer expression; if present, it shall have', &
'      the same the same value on all images and refer to an image of the', &
'      current team.', &
'', &
'  o  STAT : (optional) a scalar integer variable', &
'', &
'  o  ERRMSG : (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_co_reduce', &
'      implicit none', &
'      integer :: val', &
'', &
'         val = this_image()', &
'         call co_reduce(val, myprod, 1)', &
'         if (this_image() == 1) then', &
'            write(*,*) "Product value", val  ! prints num_images() factorial', &
'         endif', &
'', &
'      contains', &
'', &
'      pure function myprod(a, b)', &
'         integer, value :: a, b', &
'         integer :: myprod', &
'         myprod = a * b', &
'      end function myprod', &
'', &
'      end program demo_co_reduce', &
'', &
'NOTE', &
'  While the rules permit in principle an intrinsic function, none of the', &
'  intrinsics in the standard fulfill the criteria of having a specific', &
'  function, which takes two arguments of the same type and returning that type', &
'  as a result.', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  CO_MIN(3), CO_MAX(3), CO_SUM(3), CO_BROADCAST(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           co_reduce(3fortran)', &
'']

shortname="co_reduce"
call process()


case('59','cos')

textblock=[character(len=256) :: &
'', &
'cos(3fortran)                                                    cos(3fortran)', &
'', &
'NAME', &
'  COS(3) - [MATHEMATICS:TRIGONOMETRIC] Cosine function', &
'', &
'SYNOPSIS', &
'  result = cos(x)', &
'', &
'           elemental TYPE(kind=KIND) function cos(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is of type real or complex of any valid kind.', &
'', &
'  o  KIND may be any kind supported by the associated type of X.', &
'', &
'  o  The returned value will be of the same type and kind as the argument X.', &
'', &
'DESCRIPTION', &
'  COS(3) computes the cosine of an angle X given the size of the angle in', &
'  radians.', &
'', &
'  The cosine of a real value is the ratio of the adjacent side to the', &
'  hypotenuse of a right-angled triangle.', &
'', &
'OPTIONS', &
'  o  X : The angle in radians to compute the cosine of.', &
'', &
'RESULT', &
'  The return value is the tangent of X.', &
'', &
'  If X is of the type real, the return value is in radians and lies in the', &
'  range -1 <= COS(X) <= 1 .', &
'', &
'  If X is of type complex, its real part is regarded as a value in radians,', &
'  often called the phase.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_cos', &
'      implicit none', &
'      character(len=*),parameter :: g2=''(a,t20,g0)''', &
'      doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0', &
'         write(*,g2)''COS(0.0)='',cos(0.0)', &
'         write(*,g2)''COS(PI)='',cos(PI)', &
'         write(*,g2)''COS(PI/2.0d0)='',cos(PI/2.0d0),''EPSILON='',epsilon(PI)', &
'         write(*,g2)''COS(2*PI)='',cos(2*PI)', &
'         write(*,g2)''COS(-2*PI)='',cos(-2*PI)', &
'         write(*,g2)''COS(-2000*PI)='',cos(-2000*PI)', &
'         write(*,g2)''COS(3000*PI)='',cos(3000*PI)', &
'      end program demo_cos', &
'', &
'  Results:', &
'', &
'       > COS(0.0)=          1.000000', &
'       > COS(PI)=           -1.000000000000000', &
'       > COS(PI/2.0d0)=     .6123233995736766E-16', &
'       > EPSILON=           .2220446049250313E-15', &
'       > COS(2*PI)=         1.000000000000000', &
'       > COS(-2*PI)=        1.000000000000000', &
'       > COS(-2000*PI)=     1.000000000000000', &
'       > COS(3000*PI)=      1.000000000000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ACOS(3), SIN(3), TAN(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:sine and cosine', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022                 cos(3fortran)', &
'']

shortname="cos"
call process()


case('60','cosh')

textblock=[character(len=256) :: &
'', &
'cosh(3fortran)                                                  cosh(3fortran)', &
'', &
'NAME', &
'  COSH(3) - [MATHEMATICS:TRIGONOMETRIC] Hyperbolic cosine function', &
'', &
'SYNOPSIS', &
'  result = cosh(x)', &
'', &
'           elemental TYPE(kind=KIND) function cosh(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be real or complex of any kind.', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  COSH(3) computes the hyperbolic cosine of X.', &
'', &
'  If X is of type complex its imaginary part is regarded as a value in', &
'  radians.', &
'', &
'OPTIONS', &
'  o  X : the value to compute the hyperbolic cosine of', &
'', &
'RESULT', &
'  If X is complex, the imaginary part of the result is in radians.', &
'', &
'  If X is real, the return value has a lower bound of one, COSH(X) >= 1.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_cosh', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'       & real_kinds, real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 1.0_real64', &
'          write(*,*)''X='',x,''COSH(X=)'',cosh(x)', &
'      end program demo_cosh', &
'', &
'  Results:', &
'', &
'       >  X=   1.00000000000000      COSH(X=)   1.54308063481524', &
'', &
'STANDARD', &
'  FORTRAN 77 , for a complex argument - Fortran 2008', &
'', &
'SEE ALSO', &
'  Inverse function: ACOSH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:hyperbolic functions', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022                cosh(3fortran)', &
'']

shortname="cosh"
call process()


case('61','co_sum')

textblock=[character(len=256) :: &
'', &
'co_sum(3fortran)                                              co_sum(3fortran)', &
'', &
'NAME', &
'  CO_SUM(3) - [COLLECTIVE] Sum of values on the current set of images', &
'', &
'SYNOPSIS', &
'  call co_sum(a, result_image [,stat] [,errmsg] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_SUM(3) sums up the values of each element of A on all images of the', &
'  current team.', &
'', &
'  If result_image is present, the summed-up values are returned in A on the', &
'  specified image only and the value of A on the other images become', &
'  undefined.', &
'', &
'  If result_image is not present, the value is returned on all images. If the', &
'  execution was successful and STAT is present, it is assigned the value zero.', &
'  If the execution failed, STAT gets assigned a nonzero value and, if present,', &
'  ERRMSG gets assigned a value describing the occurred error.', &
'', &
'OPTIONS', &
'  o  A : shall be an integer, real or complex variable, which has the same', &
'     type and type parameters on all images of the team.', &
'', &
'  o  RESULT_IMAGE : (optional) a scalar integer expression; if present, it', &
'     shall have the same the same value on all images and refer to an image of', &
'     the current team.', &
'', &
'  o  STAT : (optional) a scalar integer variable', &
'', &
'  o  ERRMSG : (optional) a scalar character variable', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_co_sum', &
'      implicit none', &
'      integer :: val', &
'         val = this_image()', &
'         call co_sum(val, result_image=1)', &
'         if (this_image() == 1) then', &
'            ! prints (n**2 + n)/2, with n = num_images()', &
'            write(*,*) "The sum is ", val', &
'         endif', &
'      end program demo_co_sum', &
'', &
'  Results:', &
'', &
'          The sum is            1', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  CO_MAX(3), CO_MIN(3), CO_REDUCE(3), CO_BROADCAST(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              co_sum(3fortran)', &
'']

shortname="co_sum"
call process()


case('62','co_ubound')

textblock=[character(len=256) :: &
'', &
'co_ubound(3fortran)                                        co_ubound(3fortran)', &
'', &
'NAME', &
'  CO_UBOUND(3) - [COLLECTIVE] Upper codimension bounds of an array', &
'', &
'SYNOPSIS', &
'  result = co_ubound(coarray [,dim] [,kind] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  CO_UBOUND(3) returns the upper cobounds of a coarray, or a single upper', &
'  cobound along the DIM codimension.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an coarray, of any type.', &
'', &
'  o  DIM : (Optional) Shall be a scalar integer.', &
'', &
'  o  KIND : (Optional) An integer initialization expression indicating the', &
'     kind parameter of the result.', &
'', &
'RESULT', &
'  The return value is of type integer and of kind KIND. If KIND is absent, the', &
'  return value is of default integer kind. If DIM is absent, the result is an', &
'  array of the lower cobounds of COARRAY. If DIM is present, the result is a', &
'  scalar corresponding to the lower cobound of the array along that', &
'  codimension.', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  CO_LBOUND(3), LBOUND(3), UBOUND(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           co_ubound(3fortran)', &
'']

shortname="co_ubound"
call process()


case('63','count')

textblock=[character(len=256) :: &
'', &
'count(3fortran)                                                count(3fortran)', &
'', &
'NAME', &
'  COUNT(3) - [ARRAY:REDUCTION] Count true values in an array', &
'', &
'SYNOPSIS', &
'  result = count(mask [,dim] [,kind] )', &
'', &
'           integer(kind=KIND) function count(mask, dim, KIND )', &
'', &
'            logical(kind=**),intent(in) :: mask(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  MASK is a logical array of any shape and kind.', &
'', &
'  o  If DIM is present, the result is an array with the specified rank', &
'     removed.', &
'', &
'  o  KIND is a scalar integer constant expression valid as an integer kind', &
'', &
'  o  The return value is of default integer type unless KIND is specified to', &
'     declare the kind of the result.', &
'', &
'DESCRIPTION', &
'  COUNT(3) counts the number of .true. elements in a logical MASK, or, if the', &
'  DIM argument is supplied, counts the number of elements along each row of', &
'  the array in the DIM direction. If the array has zero size or all of the', &
'  elements of MASK are false, then the result is 0.', &
'', &
'OPTIONS', &
'  o  MASK : an array to count the number of .true. values in', &
'', &
'  o  DIM : specifies to remove this dimension from the result and produce an', &
'     array of counts of .true. values along the removed dimension. If not', &
'     present, the result is a scalar count of the true elements in MASK the', &
'     value must be in the range 1 <= dim <= n, where n is the rank(number of', &
'     dimensions) of MASK.', &
'', &
'     The corresponding actual argument shall not be an optional dummy', &
'     argument, a disassociated pointer, or an unallocated allocatable.', &
'', &
'  o  KIND : An integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'RESULT', &
'  The return value is the number of .true. values in MASK if DIM is not', &
'  present.', &
'', &
'  If DIM is present, the result is an array with a rank one less than the rank', &
'  of the input array MASK, and a size corresponding to the shape of ARRAY with', &
'  the DIM dimension removed, with the remaining elements containing the number', &
'  of .true. elements along the removed dimension.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_count', &
'      implicit none', &
'      character(len=*),parameter :: ints=''(*(i2,1x))''', &
'      ! two arrays and a mask all with the same shape', &
'      integer, dimension(2,3) :: a, b', &
'      logical, dimension(2,3) :: mymask', &
'      integer :: i', &
'      integer :: c(2,3,4)', &
'', &
'         print *,''the numeric arrays we will compare''', &
'         a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])', &
'         b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])', &
'         c = reshape( [( i,i=1,24)], [ 2, 3 ,4])', &
'         print ''(3i3)'', a(1,:)', &
'         print ''(3i3)'', a(2,:)', &
'         print *', &
'         print ''(3i3)'', b(1,:)', &
'         print ''(3i3)'', b(2,:)', &
'', &
'        ! basic calls', &
'         print *, ''count a few basic things creating a mask from an expression''', &
'         print *, ''count a>b'',count(a>b)', &
'         print *, ''count b<a'',count(a<b)', &
'         print *, ''count b==a'',count(a==b)', &
'         print *, ''check sum = '',count(a>b) + &', &
'                               & count(a<b) + &', &
'                               & count(a==b).eq.size(a)', &
'', &
'         ! The common usage is just getting a count, but if you want', &
'         ! to specify the DIM argument and get back reduced arrays', &
'         ! of counts this is easier to visualize if we look at a mask.', &
'         print *, ''make a mask identifying unequal elements ...''', &
'         mymask = a.ne.b', &
'         print *, ''the mask generated from a.ne.b''', &
'         print ''(3l3)'', mymask(1,:)', &
'         print ''(3l3)'', mymask(2,:)', &
'', &
'         print *,''count total and along rows and columns ...''', &
'', &
'         print ''(a)'', ''number of elements not equal''', &
'         print ''(a)'', ''(ie. total true elements in the mask)''', &
'         print ''(3i3)'', count(mymask)', &
'', &
'         print ''(a)'', ''count of elements not equal in each column''', &
'         print ''(a)'', ''(ie. total true elements in each column)''', &
'         print ''(3i3)'', count(mymask, dim=1)', &
'', &
'         print ''(a)'', ''count of elements not equal in each row''', &
'         print ''(a)'', ''(ie. total true elements in each row)''', &
'         print ''(3i3)'', count(mymask, dim=2)', &
'', &
'         ! working with rank=3 ...', &
'         print *, ''lets try this with c(2,3,4)''', &
'         print *,''  taking the result of the modulo   ''', &
'         print *,''   z=1      z=2      z=3      z=4   ''', &
'         print *,''  1 3 0 || 2 4 1 || 3 0 2 || 4 1 3 |''', &
'         print *,''  2 4 1 || 3 0 2 || 4 1 3 || 0 2 4 |''', &
'         print *,''                                    ''', &
'         print *,''  would result in the mask ..       ''', &
'         print *,''  F F T || F F F || F T F || F F F |''', &
'         print *,''  F F F || F T F || F F F || T F F |''', &
'         print *,''                                    ''', &
'         print *,'' the total number of .true.values is''', &
'         print ints, count(modulo(c,5).eq.0)', &
'         call printi(''counting up along a row and removing rows'',&', &
'         count(modulo(c,5).eq.0,dim=1))', &
'         call printi(''counting up along a column and removing columns'',&', &
'         count(modulo(c,5).eq.0,dim=2))', &
'         call printi(''counting up along a depth and removing depths'',&', &
'         count(modulo(c,5).eq.0,dim=3))', &
'', &
'      contains', &
'', &
'         ! CONVENIENCE ROUTINE FOR PRINTING SMALL INTEGER MATRICES', &
'         subroutine printi(title,arr)', &
'         implicit none', &
'', &
'         !@(#) print small 2d integer arrays in row-column format', &
'', &
'         character(len=*),parameter :: all=''(*(g0,1x))'' ! a handy format', &
'         character(len=*),intent(in)  :: title', &
'         integer,intent(in)           :: arr(:,:)', &
'         integer                      :: i', &
'         character(len=:),allocatable :: biggest', &
'', &
'            print all', &
'            print all, trim(title),'':('',shape(arr),'')''  ! print title', &
'            biggest=''           ''  ! make buffer to write integer into', &
'            ! find how many characters to use for integers', &
'            write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'            ! use this format to write a row', &
'            biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'            ! print one row of array at a time', &
'            do i=1,size(arr,dim=1)', &
'               write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'', &
'         end subroutine printi', &
'      end program demo_count', &
'', &
'  Results:', &
'', &
'       >   the numeric arrays we will compare', &
'       >    1  3  5', &
'       >    2  4  6', &
'       >', &
'       >    0  3  5', &
'       >    7  4  8', &
'       >   count a few basic things creating a mask from an expression', &
'       >   count a>b           1', &
'       >   count b<a           2', &
'       >   count b==a           3', &
'       >   check sum =  T', &
'       >   make a mask identifying unequal elements ...', &
'       >   the mask generated from a.ne.b', &
'       >    T  F  F', &
'       >    T  F  T', &
'       >   count total and along rows and columns ...', &
'       >  number of elements not equal', &
'       >  (ie. total true elements in the mask)', &
'       >    3', &
'       >  count of elements not equal in each column', &
'       >  (ie. total true elements in each column)', &
'       >    2  0  1', &
'       >  count of elements not equal in each row', &
'       >  (ie. total true elements in each row)', &
'       >    1  2', &
'       >   lets try this with c(2,3,4)', &
'       >     taking the result of the modulo', &
'       >      z=1      z=2      z=3      z=4', &
'       >     1 3 0 || 2 4 1 || 3 0 2 || 4 1 3 |', &
'       >     2 4 1 || 3 0 2 || 4 1 3 || 0 2 4 |', &
'       >', &
'       >     would result in the mask ..', &
'       >     F F T || F F F || F T F || F F F |', &
'       >     F F F || F T F || F F F || T F F |', &
'       >', &
'       >    the total number of .true.values is', &
'       >   4', &
'       >', &
'       >  counting up along a row and removing rows :( 3 4 )', &
'       >   > [ 0, 0, 0, 1 ]', &
'       >   > [ 0, 1, 1, 0 ]', &
'       >   > [ 1, 0, 0, 0 ]', &
'       >', &
'       >  counting up along a column and removing columns :( 2 4 )', &
'       >   > [ 1, 0, 1, 0 ]', &
'       >   > [ 0, 1, 0, 1 ]', &
'       >', &
'       >  counting up along a depth and removing depths :( 2 3 )', &
'       >   > [ 0, 1, 1 ]', &
'       >   > [ 1, 1, 0 ]', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  ANY(3), ALL(3), SUM(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               count(3fortran)', &
'']

shortname="count"
call process()


case('64','cpu_time')

textblock=[character(len=256) :: &
'', &
'cpu_time(3fortran)                                          cpu_time(3fortran)', &
'', &
'NAME', &
'  CPU_TIME(3) - [SYSTEM:TIME] Return CPU processor time used in seconds', &
'', &
'SYNOPSIS', &
'  call cpu_time(time)', &
'', &
'            subroutine cpu_time(time)', &
'', &
'             real,intent(out) :: time', &
'', &
'CHARACTERISTICS', &
'  o  TIME is a real of any kind', &
'', &
'DESCRIPTION', &
'  CPU_TIME(3) returns a real value representing the elapsed CPU time in', &
'  seconds. This is useful for testing segments of code to determine execution', &
'  time.', &
'', &
'  If no time source is available, TIME is set to a negative value.', &
'', &
'  The exact definition of time is left imprecise because of the variability in', &
'  what different processors are able to provide.', &
'', &
'  Note that TIME may contain a system dependent, arbitrary offset and may not', &
'  start with 0.0. For CPU_TIME(3) the absolute value is meaningless.  Only', &
'  differences between subsequent calls, as shown in the example below, should', &
'  be used.', &
'', &
'PARALLEL PROCESSING', &
'  Whether the value assigned is an approximation to the amount of time used by', &
'  the invoking image, or the amount of time used by the whole program, is', &
'  processor dependent.', &
'', &
'  A processor for which a single result is inadequate (for example, a parallel', &
'  processor) might choose to provide an additional version for which TIME is', &
'  an array.', &
'', &
'RESULT', &
'  o  TIME : is assigned a processor-dependent approximation to the processor', &
'     time in seconds. If the processor cannot return a meaningful time, a', &
'     processor-dependent negative value is returned.', &
'', &
'     : The start time is left imprecise because the purpose is to time', &
'     sections of code, as in the example. This might or might not include', &
'     system overhead time.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_cpu_time', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128', &
'      implicit none', &
'      real :: start, finish', &
'      real(kind=real64) :: startd, finishd', &
'         !', &
'         call cpu_time(start)', &
'         call cpu_time(startd)', &
'         ! put code to time here', &
'         call cpu_time(finish)', &
'         call cpu_time(finishd)', &
'         !', &
'        ! writes processor time taken by the piece of code.', &
'', &
'        ! the accuracy of the clock and whether it includes system time', &
'        ! as well as user time is processor dependent. Accuracy up to', &
'        ! milliseconds is common but not guaranteed, and may be much', &
'        ! higher or lower', &
'         print ''("Processor Time = ",f6.3," seconds.")'',finish-start', &
'', &
'         ! see your specific compiler documentation for how to measure', &
'         ! parallel jobs and for the precision of the time returned', &
'         print ''("Processor Time = ",g0," seconds.")'',finish-start', &
'         print ''("Processor Time = ",g0," seconds.")'',finishd-startd', &
'      end program demo_cpu_time', &
'', &
'  Results:', &
'', &
'  The precision of the result, some aspects of what is returned, and what if', &
'  any options there are for parallel applications may very from system to', &
'  system. See compiler-specific for details.', &
'', &
'         Processor Time =  0.000 seconds.', &
'         Processor Time = .4000030E-05 seconds.', &
'         Processor Time = .2000000000000265E-05 seconds.', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  SYSTEM_CLOCK(3), DATE_AND_TIME(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022            cpu_time(3fortran)', &
'']

shortname="cpu_time"
call process()


case('65','cshift')

textblock=[character(len=256) :: &
'', &
'cshift(3fortran)                                              cshift(3fortran)', &
'', &
'NAME', &
'  CSHIFT(3) - [TRANSFORMATIONAL] Circular shift elements of an array', &
'', &
'SYNOPSIS', &
'  result = cshift(array, shift [,dim])', &
'', &
'          type(TYPE, kind=KIND) function cshift(array, shift, dim )', &
'', &
'           type(TYPE,kind=KIND),intent(in) :: array(..)', &
'           integer(kind=**),intent(in)  :: shift', &
'           integer(kind=**),intent(in)  :: dim', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY may be any type and rank', &
'', &
'  o  SHIFT an integer scalar if ARRAY has rank one. Otherwise, it shall be', &
'     scalar or of rank n-1 and of shape [d1, d2, ..., dDIM-1, dDIM+1,', &
'', &
'  o  DIM is an integer scalar with a value in the range 1 <= DIM <= n, where n', &
'     is the rank of ARRAY. If DIM is absent, it is as if it were present with', &
'     the value 1.', &
'', &
'  o  the result will automatically be of the same type, kind and shape as', &
'     ARRAY.', &
'', &
'  NOTE: :a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  CSHIFT(3) performs a circular shift on elements of ARRAY along the dimension', &
'  of DIM. If DIM is omitted it is taken to be 1. DIM is a scalar of type', &
'  integer in the range of 1 <= DIM <= N, where "n" is the rank of ARRAY.', &
'', &
'  If the rank of ARRAY is one, then all elements of ARRAY are shifted by SHIFT', &
'  places. If rank is greater than one, then all complete rank one sections of', &
'  ARRAY along the given dimension are shifted. Elements shifted out one end of', &
'  each rank one section are shifted back in the other end.', &
'', &
'OPTIONS', &
'  o  ARRAY : An array of any type which is to be shifted', &
'', &
'  o  SHIFT : the number of positions to circularly shift. A negative value', &
'     produces a right shift, a positive value produces a left shift.', &
'', &
'  o  DIM : the dimension along which to shift a multi-rank ARRAY.  Defaults to', &
'     1.', &
'', &
'RESULT', &
'  Returns an array of same type and rank as the ARRAY argument.', &
'', &
'  The rows of an array of rank two may all be shifted by the same amount or by', &
'  different amounts.', &
'', &
'    cshift', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_cshift', &
'      implicit none', &
'      integer, dimension(5)   :: i1,i2,i3', &
'      integer, dimension(3,4) :: a, b', &
'         !basics', &
'          i1=[10,20,30,40,50]', &
'          print *,''start with:''', &
'          print ''(1x,5i3)'', i1', &
'          print *,''shift -2''', &
'          print ''(1x,5i3)'', cshift(i1,-2)', &
'          print *,''shift +2''', &
'          print ''(1x,5i3)'', cshift(i1,+2)', &
'', &
'          print *,''start with a matrix''', &
'          a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ], [ 3, 4 ])', &
'          print ''(4i3)'', a(1,:)', &
'          print ''(4i3)'', a(2,:)', &
'          print ''(4i3)'', a(3,:)', &
'          print *,''matrix shifted along rows, each by its own amount [-1,0,1]''', &
'          b = cshift(a, SHIFT=[1, 0, -1], DIM=2)', &
'          print *', &
'          print ''(4i3)'', b(1,:)', &
'          print ''(4i3)'', b(2,:)', &
'          print ''(4i3)'', b(3,:)', &
'      end program demo_cshift', &
'', &
'  Results:', &
'', &
'       >  start with:', &
'       >   10 20 30 40 50', &
'       >  shift -2', &
'       >   40 50 10 20 30', &
'       >  shift +2', &
'       >   30 40 50 10 20', &
'       >  start with a matrix', &
'       >   1  4  7 10', &
'       >   2  5  8 11', &
'       >   3  6  9 12', &
'       >  matrix shifted along rows, each by its own amount', &
'       >', &
'       >   4  7 10  1', &
'       >   2  5  8 11', &
'       >  12  3  6  9', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  EOSHIFT(3) - End-off shift elements of an array', &
'', &
'  o  SUM(3) - sum the elements of an array', &
'', &
'  o  PRODUCT(3) - Product of array elements', &
'', &
'  o  FINDLOC(3) - Location of first element of ARRAY identified by MASK along', &
'     dimension DIM having a value', &
'', &
'  o  MAXLOC(3) - Location of the maximum value within an array', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              cshift(3fortran)', &
'']

shortname="cshift"
call process()


case('66','c_sizeof')

textblock=[character(len=256) :: &
'', &
'c_sizeof(3fortran)                                          c_sizeof(3fortran)', &
'', &
'NAME', &
'  C_SIZEOF(3) - [ISO_C_BINDING] Size in bytes of an expression', &
'', &
'SYNOPSIS', &
'  result = c_sizeof(x)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  C_SIZEOF(3) calculates the number of bytes of storage the expression X', &
'  occupies.', &
'', &
'OPTIONS', &
'  o  X : The argument shall be an interoperable data entity.', &
'', &
'RESULT', &
'  The return value is of type integer and of the system-dependent kind csize_t', &
'  (from the iso_c_binding module). Its value is the number of bytes occupied', &
'  by the argument. If the argument has the pointer attribute, the number of', &
'  bytes of the storage area pointed to is returned. If the argument is of a', &
'  derived type with pointer or allocatable components, the return value does', &
'  not account for the sizes of the data pointed to by these components.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_c_sizeof', &
'      use iso_c_binding', &
'      implicit none', &
'      real(c_float) :: r, s(5)', &
'         print *, (c_sizeof(s)/c_sizeof(r) == 5)', &
'      end program demo_c_sizeof', &
'', &
'  Results:', &
'', &
'   T', &
'  The example will print .true. unless you are using a platform where default', &
'  real variables are unusually padded.', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  STORAGE_SIZE(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022            c_sizeof(3fortran)', &
'']

shortname="c_sizeof"
call process()


case('67','date_and_time')

textblock=[character(len=256) :: &
'', &
'date_and_time(3fortran)                                date_and_time(3fortran)', &
'', &
'NAME', &
'  DATE_AND_TIME(3) - [SYSTEM:TIME] Gets current date time', &
'', &
'SYNOPSIS', &
'  call date_and_time( [date] [,time] [,zone] [,values] )', &
'', &
'           subroutine date_and_time(date, time, zone, values)', &
'', &
'            character(len=8),intent(out),optional :: date', &
'            character(len=10),intent(out),optional :: time', &
'            character(len=5),intent(out),optional :: zone', &
'            integer,intent(out),optional :: values(8)', &
'', &
'CHARACTERISTICS', &
'  o  *date is a default character scalar', &
'', &
'  o  *time is a default character scalar', &
'', &
'  o  *zone is a default character scalar', &
'', &
'  o  VALUES is a rank-one array of type integer with a decimal exponent range', &
'     of at least four.', &
'', &
'DESCRIPTION', &
'  DATE_AND_TIME(3) gets the corresponding date and time information from the', &
'  real-time system clock.', &
'', &
'  Unavailable time and date character parameters return blanks.', &
'', &
'  Unavailable numeric parameters return -HUGE(VALUE).', &
'', &
'  These forms are compatible with the representations defined in ISO', &
'  8601:2004. UTC is established by the International Bureau of Weights and', &
'  Measures (BIPM, i.e. Bureau International des Poids et Mesures) and the', &
'  International Earth Rotation Service (IERS).', &
'', &
'OPTIONS', &
'  o  DATE : A character string of default kind of the form CCYYMMDD, of length', &
'     8 or larger, where', &
'', &
'          + CCYY is the year in the Gregorian calendar', &
'          + MM is the month within the year', &
'          + DD is the day within the month.', &
'', &
'  The characters of this value are all decimal digits.', &
'', &
'    If there is no date available, DATE is assigned all blanks.', &
'', &
'  o  TIME : A character string of default kind of the form HHMMSS.SSS, of', &
'     length 10 or larger, where', &
'', &
'     o  hh is the hour of the day,', &
'', &
'     o  mm is the minutes of the hour,', &
'', &
'     o  and ss.sss is the seconds and milliseconds of the minute.', &
'', &
'     Except for the decimal point, the characters of this value shall all be', &
'     decimal digits.', &
'', &
'     If there is no clock available, TIME is assigned all blanks.', &
'', &
'  o  ZONE : A string of the form (+-)HHMM, of length 5 or larger, representing', &
'     the difference with respect to Coordinated Universal Time (UTC), where', &
'', &
'     o  hh and mm are the time difference with respect to Coordinated', &
'        Universal Time (UTC) in hours and minutes, respectively.', &
'', &
'     The characters of this value following the sign character are all decimal', &
'     digits.', &
'', &
'     If this information is not available, ZONE is assigned all blanks.', &
'', &
'  o  VALUES : An array of at least eight elements. If there is no data', &
'     available for a value it is set to -HUGE(VALUES). Otherwise, it contains:', &
'', &
'     o  VALUES(1) : The year, including the century.', &
'', &
'     o  VALUES(2) : The month of the year', &
'', &
'     o  VALUES(3) : The day of the month', &
'', &
'     o  VALUES(4) : Time difference in minutes between the reported time and', &
'        UTC time.', &
'', &
'     o  VALUES(5) : The hour of the day, in the range 0 to 23.', &
'', &
'     o  VALUES(6) : The minutes of the hour, in the range 0 to 59', &
'', &
'     o  VALUES(7) : The seconds of the minute, in the range 0 to 60', &
'', &
'     o  VALUES(8) : The milliseconds of the second, in the range 0 to 999.', &
'', &
'  The date, clock, and time zone information might be available on some images', &
'  and not others. If the date, clock, or time zone information is available on', &
'  more than one image, it is processor dependent whether or not those images', &
'  share the same information.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_date_and_time', &
'      implicit none', &
'      character(len=8)     :: date', &
'      character(len=10)    :: time', &
'      character(len=5)     :: zone', &
'      integer,dimension(8) :: values', &
'', &
'          call date_and_time(date,time,zone,values)', &
'', &
'          ! using keyword arguments', &
'          call date_and_time(DATE=date,TIME=time,ZONE=zone)', &
'          print ''(*(g0))'',''DATE="'',date,''" TIME="'',time,''" ZONE="'',zone,''"''', &
'', &
'          call date_and_time(VALUES=values)', &
'          write(*,''(i5,a)'') &', &
'           & values(1),'' - The year'', &', &
'           & values(2),'' - The month'', &', &
'           & values(3),'' - The day of the month'', &', &
'           & values(4),'' - Time difference with UTC in minutes'', &', &
'           & values(5),'' - The hour of the day'', &', &
'           & values(6),'' - The minutes of the hour'', &', &
'           & values(7),'' - The seconds of the minute'', &', &
'           & values(8),'' - The milliseconds of the second''', &
'      end program demo_date_and_time', &
'', &
'  Results:', &
'', &
'       > DATE="20201222" TIME="165738.779" ZONE="-0500"', &
'       >  2020 - The year', &
'       >    12 - The month', &
'       >    22 - The day of the month', &
'       >  -300 - Time difference with UTC in minutes', &
'       >    16 - The hour of the day', &
'       >    57 - The minutes of the hour', &
'       >    38 - The seconds of the minute', &
'       >   779 - The milliseconds of the second', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  CPU_TIME(3), SYSTEM_CLOCK(3)', &
'', &
'RESOURCES', &
'  date and time conversion, formatting and computation', &
'', &
'  o  M_time - https://github.com/urbanjost/M_time', &
'', &
'  o  fortran-datetime https://github.com/dongli/fortran-datetime', &
'', &
'  o  datetime-fortran - https://github.com/wavebitscientific/datetime-fortran', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022       date_and_time(3fortran)', &
'']

shortname="date_and_time"
call process()


case('68','dble')

textblock=[character(len=256) :: &
'', &
'dble(3fortran)                                                  dble(3fortran)', &
'', &
'NAME', &
'  DBLE(3) - [TYPE:NUMERIC] Converstion to double precision real', &
'', &
'SYNOPSIS', &
'  result = dble(a)', &
'', &
'           elemental doubleprecision function dble(a)', &
'', &
'            doubleprecision :: dble', &
'            TYPE(kind=KIND),intent(in) :: a', &
'', &
'CHARACTERISTICS', &
'  o  A my be integer, real, complex, or a BOZ-literal-constant', &
'', &
'  o  the result is a doubleprecision real.', &
'', &
'DESCRIPTION', &
'  DBLE(3) Converts A to double precision real type.', &
'', &
'OPTIONS', &
'  o  A : a value to convert to a doubleprecision real.', &
'', &
'RESULT', &
'  The return value is of type doubleprecision. For complex input, the returned', &
'  value has the magnitude and sign of the real component of the input value.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_dble', &
'      implicit none', &
'      real:: x = 2.18', &
'      integer :: i = 5', &
'      complex :: z = (2.3,1.14)', &
'         print *, dble(x), dble(i), dble(z)', &
'      end program demo_dble', &
'', &
'  Results:', &
'', &
'        2.1800000667572021  5.0000000000000000   2.2999999523162842', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  AIMAG(3) - Imaginary part of complex number', &
'', &
'  o  CMPLX(3) - Convert values to a complex type', &
'', &
'  o  INT(3) - Truncate towards zero and convert to integer', &
'', &
'  o  NINT(3) - Nearest whole number', &
'', &
'  o  OUT_OF_RANGE(3) - Whether a value cannot be converted safely.', &
'', &
'  o  REAL(3) - Convert to real type', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                dble(3fortran)', &
'']

shortname="dble"
call process()


case('69','digits')

textblock=[character(len=256) :: &
'', &
'digits(3fortran)                                              digits(3fortran)', &
'', &
'NAME', &
'  DIGITS(3) - [NUMERIC MODEL] Significant digits in the numeric model', &
'', &
'SYNOPSIS', &
'  result = digits(x)', &
'', &
'           integer function digits(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x(..)', &
'', &
'CHARACTERISTICS', &
'  o  X an integer or ral scalar or array', &
'', &
'  o  The return value is an integer of default kind.', &
'', &
'DESCRIPTION', &
'  DIGITS(3) returns the number of significant digits of the internal model', &
'  representation of X. For example, on a system using a 32-bit floating point', &
'  representation, a default real number would likely return 24.', &
'', &
'OPTIONS', &
'  o  X : a value of the type and kind to query', &
'', &
'RESULT', &
'  The number of significant digits in a variable of the type and kind of X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_digits', &
'      implicit none', &
'      integer :: i = 12345', &
'      real :: x = 3.143', &
'      doubleprecision :: y = 2.33d0', &
'         print *,''default integer:'', digits(i)', &
'         print *,''default real:   '', digits(x)', &
'         print *,''default doubleprecision:'', digits(y)', &
'      end program demo_digits', &
'', &
'  Results:', &
'', &
'       >  default integer:          31', &
'       >  default real:             24', &
'       >  default doubleprecision:          53', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              digits(3fortran)', &
'']

shortname="digits"
call process()


case('70','dim')

textblock=[character(len=256) :: &
'', &
'dim(3fortran)                                                    dim(3fortran)', &
'', &
'NAME', &
'  DIM(3) - [NUMERIC] Positive difference of X - Y', &
'', &
'SYNOPSIS', &
'  result = dim(x, y)', &
'', &
'           elemental TYPE(kind=KIND) function dim(x, y )', &
'', &
'            TYPE(kind=KIND),intent(in) :: x, y', &
'', &
'CHARACTERISTICS', &
'  o  X and Y may be any real or integer but of the same type and kind', &
'', &
'  o  the result is of the same type and kind as the arguments', &
'', &
'DESCRIPTION', &
'  DIM(3) returns the maximum of X - Y and zero. That is, it returns the', &
'  difference X - Y if the result is positive; otherwise it returns zero.  It', &
'  is equivalent to', &
'', &
'        max(0,x-y)', &
'', &
'OPTIONS', &
'  o  X : the subtrahend, ie. the number being subtracted from.', &
'', &
'  o  Y : the minuend; ie. the number being subtracted', &
'', &
'RESULT', &
'  Returns the difference X - Y or zero, whichever is larger.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_dim', &
'      use, intrinsic :: iso_fortran_env, only : real64', &
'      implicit none', &
'      integer           :: i', &
'      real(kind=real64) :: x', &
'', &
'         ! basic usage', &
'          i = dim(4, 15)', &
'          x = dim(4.321_real64, 1.111_real64)', &
'          print *, i', &
'          print *, x', &
'', &
'         ! elemental', &
'          print *, dim([1,2,3],2)', &
'          print *, dim([1,2,3],[3,2,1])', &
'          print *, dim(-10,[0,-10,-20])', &
'', &
'      end program demo_dim', &
'', &
'  Results:', &
'', &
'       >            0', &
'       >    3.21000000000000', &
'       >            0           0           1', &
'       >            0           0           2', &
'       >            0           0          10', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 dim(3fortran)', &
'']

shortname="dim"
call process()


case('71','dot_product')

textblock=[character(len=256) :: &
'', &
'dot_product(3fortran)                                    dot_product(3fortran)', &
'', &
'NAME', &
'  DOT_PRODUCT(3) - [TRANSFORMATIONAL] Dot product of two vectors', &
'', &
'SYNOPSIS', &
'  result = dot_product(vector_a, vector_b)', &
'', &
'           TYPE(kind=KIND) function dot_product(vector_a, vector_b)', &
'', &
'            TYPE(kind=KIND),intent(in) :: vector_a(:)', &
'            TYPE(kind=KIND),intent(in) :: vector_b(:)', &
'', &
'CHARACTERISTICS', &
'  o  VECTOR_A, VECTOR_B may be any numeric or logical type array of rank one', &
'     of the same size', &
'', &
'  o  the two vectors need not be of the same kind, but both must be logical or', &
'     numeric for any given call.', &
'', &
'  o  the result is the same type and kind of the vector that is the higher', &
'     type that the other vector is optionally promoted to if they differ.', &
'', &
'  The two vectors may be either numeric or logical and must be arrays of rank', &
'  one and of equal size.', &
'', &
'DESCRIPTION', &
'  DOT_PRODUCT(3) computes the dot product multiplication of two vectors', &
'  VECTOR_A and VECTOR_B.', &
'', &
'OPTIONS', &
'  o  VECTOR_A : A rank 1 vector of values', &
'', &
'  o  VECTOR_B : The type shall be numeric if VECTOR_A is of numeric type or', &
'     logical if vector_a is of type logical. vector_b shall be a rank-one', &
'     array of the same size as VECTOR_A.', &
'', &
'RESULT', &
'  If the arguments are numeric, the return value is a scalar of numeric type.', &
'  If the arguments are logical, the return value is .true. or .false..', &
'', &
'  If the vectors are integer or real, the result is', &
'', &
'           sum(vector_a*vector_b)', &
'', &
'  If the vectors are complex, the result is', &
'', &
'           sum(conjg(vector_a)*vector_b)**', &
'', &
'  If the vectors are logical, the result is', &
'', &
'           any(vector_a .and. vector_b)', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_dot_prod', &
'      implicit none', &
'          integer, dimension(3) :: a, b', &
'          a = [ 1, 2, 3 ]', &
'          b = [ 4, 5, 6 ]', &
'          print ''(3i3)'', a', &
'          print *', &
'          print ''(3i3)'', b', &
'          print *', &
'          print *, dot_product(a,b)', &
'      end program demo_dot_prod', &
'', &
'  Results:', &
'', &
'        >  1  2  3', &
'        >', &
'        >  4  5  6', &
'        >', &
'        >           32', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  SUM(3), CONJG(3), ANY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022         dot_product(3fortran)', &
'']

shortname="dot_product"
call process()


case('72','dprod')

textblock=[character(len=256) :: &
'', &
'dprod(3fortran)                                                dprod(3fortran)', &
'', &
'NAME', &
'  DPROD(3) - [NUMERIC] Double precision real product', &
'', &
'SYNOPSIS', &
'  result = dprod(x,y)', &
'', &
'           elemental function dprod(x,y)', &
'', &
'            real,intent(in) :: x', &
'            real,intent(in) :: y', &
'            doubleprecision :: dprod', &
'', &
'CHARACTERISTICS', &
'  o  X is a default real.', &
'', &
'  o  Y is a default real.', &
'', &
'  o  the result is a doubleprecision real.', &
'', &
'  The setting of compiler options specifying the size of a default real can', &
'  affect this function.', &
'', &
'DESCRIPTION', &
'  DPROD(3) produces a doubleprecision product of default real values X and Y.', &
'', &
'  That is, it is expected to convert the arguments to double precision before', &
'  multiplying, which a simple expression X*Y would not be required to do. This', &
'  can be significant in specialized computations requiring high precision.', &
'', &
'  The result has a value equal to a processor-dependent approximation to the', &
'  product of X and Y. Note it is recommended in the standard that the', &
'  processor compute the product in double precision, rather than in single', &
'  precision then converted to double precision; but is only a recommendation.', &
'', &
'OPTIONS', &
'  o  X : the multiplier', &
'', &
'  o  Y : the multiplicand', &
'', &
'RESULT', &
'  The returned value of the product should have the same value as', &
'  DBLE(X)*DBLE(Y).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_dprod', &
'      implicit none', &
'      integer,parameter :: dp=kind(0.0d0)', &
'      real :: x = 5.2', &
'      real :: y = 2.3', &
'      doubleprecision :: xx', &
'      real(kind=dp)   :: dd', &
'', &
'         print *,''algebraically 5.2 x 2.3 is exactly 11.96''', &
'         print *,''as floating point values results may differ slightly:''', &
'         ! basic usage', &
'         dd = dprod(x,y)', &
'         print *, ''compare dprod(xy)='',dd, &', &
'         & ''to x*y='',x*y, &', &
'         & ''to dble(x)*dble(y)='',dble(x)*dble(y)', &
'', &
'         print *,''test if an expected result is produced''', &
'         xx=-6.0d0', &
'         write(*,*)DPROD(-3.0, 2.0),xx', &
'         write(*,*)merge(''PASSED'',''FAILED'',DPROD(-3.0, 2.0) == xx)', &
'', &
'         print *,''elemental''', &
'         print *, dprod( [2.3,3.4,4.5], 10.0 )', &
'         print *, dprod( [2.3,3.4,4.5], [9.8,7.6,5.4] )', &
'', &
'      end program demo_dprod', &
'', &
'  Results: (this can vary between programming environments):', &
'', &
'       >  algebraically 5.2 x 2.3 is exactly 11.96', &
'       >  as floating point values results may differ slightly:', &
'       >  compare dprod(xy)=   11.9599993133545      to x*y=   11.96000', &
'       >  to dble(x)*dble(y)=   11.9599993133545', &
'       >  test if an expected result is produced', &
'       >   -6.00000000000000       -6.00000000000000', &
'       >  PASSED', &
'       >  elemental', &
'       >    22.9999995231628     34.0000009536743     45.0000000000000', &
'       >    22.5399999713898     25.8400004005432     24.3000004291534', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  DBLE(3) REAL(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               dprod(3fortran)', &
'']

shortname="dprod"
call process()


case('73','dshiftl')

textblock=[character(len=256) :: &
'', &
'dshiftl(3fortran)                                            dshiftl(3fortran)', &
'', &
'NAME', &
'  DSHIFTL(3) - [BIT:COPY] Combined left shift of the bits of two integers', &
'', &
'SYNOPSIS', &
'  result = dshiftl(i, j, shift)', &
'', &
'           elemental integer(kind=KIND) function dshiftl(i, j, shift)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=KIND),intent(in) :: j', &
'            integer(kind=**),intent(in) :: shift', &
'', &
'CHARACTERISTICS', &
'  o  the kind of I, J, and the return value are the same. An exception is that', &
'     one of I and J may be a BOZ literal constant (A BOZ literal constant is a', &
'     binary, octal or hex constant).', &
'', &
'  o  If either I or J is a BOZ-literal-constant (but not both), it is first', &
'     converted as if by the intrinsic function INT(3) to type integer with the', &
'     kind type parameter of the other.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  DSHIFTL(3) combines bits of I and J. The rightmost SHIFT bits of the result', &
'  are the leftmost SHIFT bits of J, and the remaining bits are the rightmost', &
'  BITSIZE(I)-SHIFT of I.', &
'', &
'  Hence DSHIFTL is designated as a "combined left shift", because it is like', &
'  we appended I and J together, shifted it SHIFT bits to the left, and then', &
'  kept the same number of bits as I or J had.', &
'', &
'  For example, for two 16-bit values if SHIFT=6', &
'', &
'            SHIFT=6', &
'            I =             1111111111111111', &
'            J =             0000000000000000', &
'            COMBINED        11111111111111110000000000000000', &
'            DROP LEFT BITS  11111111110000000000000000', &
'            KEEP LEFT 16    1111111111000000', &
'', &
'NOTE', &
'  This is equivalent to', &
'', &
'           ior( shiftl(i, shift), shiftr(j, bit_size(j) - shift) )', &
'', &
'  Also note that using this last representation of the operation is can be', &
'  derived that when both I and J have the same value as in', &
'', &
'            dshiftl(i, i, shift)', &
'', &
'  the result has the same value as a circular shift:', &
'', &
'            ishftc(i, shift)', &
'', &
'OPTIONS', &
'  o  I : used to define the left pattern of bits in the combined pattern', &
'', &
'  o  J : used for the right pattern of bits in the combined pattern', &
'', &
'  o  SHIFT : shall be nonnegative and less than or equal to the number of bits', &
'     in an integer input value (ie. the bit size of either one that is not a', &
'     BOZ literal constant).', &
'', &
'RESULT', &
'  The leftmost SHIFT bits of J are copied to the rightmost bits of the result,', &
'  and the remaining bits are the rightmost bits of I.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_dshiftl', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int32) :: i, j', &
'      integer             :: shift', &
'', &
'        ! basic usage', &
'         write(*,*) dshiftl (1, 2**30, 2) ! int32 values on little-endian => 5', &
'', &
'        ! print some simple calls as binary to better visual the results', &
'         i=-1', &
'         j=0', &
'         shift=5', &
'         call printit()', &
'', &
'         ! the leftmost SHIFT bits of J are copied to the rightmost result bits', &
'         j=int(b"11111000000000000000000000000000")', &
'         ! and the other bits are the rightmost bits of I', &
'         i=int(b"00000000000000000000000000000000")', &
'         call printit()', &
'', &
'         j=int(b"11111000000000000000000000000000")', &
'         i=int(b"00000111111111111111111111111111")', &
'         ! result should be all 1s', &
'         call printit()', &
'', &
'      contains', &
'      subroutine printit()', &
'         ! print i,j,shift and then i,j, and the result as binary values', &
'          write(*,''(*(g0))'')''I='',i,'' J='',j,'' SHIFT='',shift', &
'          write(*,''(b32.32)'') i,j, dshiftl (i, j, shift)', &
'      end subroutine printit', &
'', &
'      end program demo_dshiftl', &
'', &
'  Results:', &
'', &
'         > I=-1 J=0 SHIFT=5', &
'         > 11111111111111111111111111111111', &
'         > 00000000000000000000000000000000', &
'         > 11111111111111111111111111100000', &
'         > I=0 J=-134217728 SHIFT=5', &
'         > 00000000000000000000000000000000', &
'         > 11111000000000000000000000000000', &
'         > 00000000000000000000000000011111', &
'         > I=134217727 J=-134217728 SHIFT=5', &
'         > 00000111111111111111111111111111', &
'         > 11111000000000000000000000000000', &
'         > 11111111111111111111111111111111', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  DSHIFTR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             dshiftl(3fortran)', &
'']

shortname="dshiftl"
call process()


case('74','dshiftr')

textblock=[character(len=256) :: &
'', &
'dshiftr(3fortran)                                            dshiftr(3fortran)', &
'', &
'NAME', &
'  DSHIFTR(3) - [BIT:COPY] Combined right shift of the bits of two integers', &
'', &
'SYNOPSIS', &
'  result = dshiftr(i, j, shift)', &
'', &
'           elemental integer(kind=KIND) function dshiftr(i, j, shift)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=KIND),intent(in) :: j', &
'            integer(kind=**),intent(in) :: shift', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any kind value for the integer type', &
'', &
'  o  the kind of I, J, and the return value are the same. An exception is that', &
'     one of I and J may be a BOZ literal constant (A BOZ literal constant is a', &
'     binary, octal or hex constant).', &
'', &
'  o  If either I or J is a BOZ-literal-constant, it is first converted as if', &
'     by the intrinsic function INT(3) to type integer with the kind type', &
'     parameter of the other.', &
'', &
'DESCRIPTION', &
'  DSHIFTR(3) combines bits of I and J. The leftmost SHIFT bits of the result', &
'  are the rightmost SHIFT bits of I, and the remaining bits are the leftmost', &
'  bits of J.', &
'', &
'  It may be thought of as appending the bits of I and J, dropping off the', &
'  SHIFT rightmost bits, and then retaining the same number of rightmost bits', &
'  as an input value, hence the name "combined right shift"...', &
'', &
'  Given two 16-bit values labeled alphabetically ...', &
'', &
'         i=ABCDEFGHIJKLMNOP', &
'         j=abcdefghijklmnop', &
'', &
'  Append them together', &
'', &
'         ABCDEFGHIJKLMNOPabcdefghijklmnop', &
'', &
'  Shift them N=6 bits to the right dropping off bits', &
'', &
'               ABCDEFGHIJKLMNOPabcdefghij', &
'', &
'  Keep the 16 right-most bits', &
'', &
'                         KLMNOPabcdefghij', &
'', &
'NOTE', &
'  DSHIFR(I,J,SHIFT) is equivalent to', &
'', &
'           ior(shiftl (i, bit_size(i) - shift), shiftr(j, shift) )', &
'', &
'  it can also be seen that if I and J have the same value', &
'', &
'           dshiftr( i, i, shift )', &
'', &
'  this has the same result as a negative circular shift', &
'', &
'           ishftc( i,   -shift ).', &
'', &
'OPTIONS', &
'  o  I : left value of the pair of values to be combine-shifted right', &
'', &
'  o  J : right value of the pair of values to be combine-shifted right', &
'', &
'  o  SHIFT : the shift value is non-negative and less than or equal to the', &
'     number of bits in an input value as can be computed by BIT_SIZE(3).', &
'', &
'RESULT', &
'  The result is a combined right shift of I and J that is the same as the bit', &
'  patterns of the inputs being combined left to right, dropping off SHIFT bits', &
'  on the right and then retaining the same number of bits as an input value', &
'  from the rightmost bits.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_dshiftr', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int32) :: i, j', &
'      integer             :: shift', &
'', &
'        ! basic usage', &
'         write(*,*) dshiftr (1, 2**30, 2)', &
'', &
'        ! print some calls as binary to better visualize the results', &
'         i=-1', &
'         j=0', &
'         shift=5', &
'', &
'         ! print values', &
'          write(*,''(*(g0))'')''I='',i,'' J='',j,'' SHIFT='',shift', &
'          write(*,''(b32.32)'') i,j, dshiftr (i, j, shift)', &
'', &
'        ! visualizing a "combined right shift" ...', &
'         i=int(b"00000000000000000000000000011111")', &
'         j=int(b"11111111111111111111111111100000")', &
'         ! appended together ( i//j )', &
'         ! 0000000000000000000000000001111111111111111111111111111111100000', &
'         ! shifted right SHIFT values dropping off shifted values', &
'         !      00000000000000000000000000011111111111111111111111111111111', &
'         ! keep enough rightmost bits to fill the kind', &
'         !                                 11111111111111111111111111111111', &
'         ! so the result should be all 1s bits ...', &
'', &
'          write(*,''(*(g0))'')''I='',i,'' J='',j,'' SHIFT='',shift', &
'          write(*,''(b32.32)'') i,j, dshiftr (i, j, shift)', &
'', &
'      end program demo_dshiftr', &
'', &
'  Results:', &
'', &
'       >    1342177280', &
'       >  I=-1 J=0 SHIFT=5', &
'       >  11111111111111111111111111111111', &
'       >  00000000000000000000000000000000', &
'       >  11111000000000000000000000000000', &
'       >  I=31 J=-32 SHIFT=5', &
'       >  00000000000000000000000000011111', &
'       >  11111111111111111111111111100000', &
'       >  11111111111111111111111111111111', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  DSHIFTL(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             dshiftr(3fortran)', &
'']

shortname="dshiftr"
call process()


case('75','eoshift')

textblock=[character(len=256) :: &
'', &
'eoshift(3fortran)                                            eoshift(3fortran)', &
'', &
'NAME', &
'  EOSHIFT(3) - [TRANSFORMATIONAL] End-off shift of elements of an array', &
'', &
'SYNOPSIS', &
'  result = eoshift( array, shift [,boundary] [,dim] )', &
'', &
'         type(TYPE(kind=KIND)) function eoshift(array,shift,boundary,dim)', &
'', &
'          type(TYPE(kind=KIND)),intent(in) :: array(..)', &
'          integer(kind=**),intent(in)      :: shift(..)', &
'          type(TYPE(kind=KIND)),intent(in) :: boundary(..)', &
'          integer(kind=**),intent(in)      :: dim', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY an array of any type', &
'', &
'  o  SHIFT is an integer of any kind. It may be a scalar. If the rank of ARRAY', &
'     is greater than one, and DIM is specified it is the same shape as ARRAY', &
'     reduced by removing dimension DIM.', &
'', &
'  o  BOUNDARY May be a scalar of the same type and kind as ARRAY. It must be a', &
'     scalar when ARRAY has a rank of one. Otherwise, it may be an array of the', &
'     same shape as ARRAY reduced by dimension DIM. It may only be absent for', &
'     certain types, as described below.', &
'', &
'  o  DIM is an integer of any kind. It defaults to one.', &
'', &
'  o  the result has the same type, type parameters, and shape as ARRAY.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  The result is an array of same type, kind and rank as the ARRAY argument.', &
'', &
'DESCRIPTION', &
'  EOSHIFT(3) performs an end-off shift on elements of ARRAY along the', &
'  dimension of DIM.', &
'', &
'  Elements shifted out one end of each rank one section are dropped.', &
'', &
'  If BOUNDARY is present then the corresponding value from BOUNDARY is copied', &
'  back in the other end, else default values are used.', &
'', &
'OPTIONS', &
'  o  ARRAY : array of any type whose elements are to be shifted. If the rank', &
'     of ARRAY is one, then all elements of ARRAY are shifted by SHIFT places.', &
'     If rank is greater than one, then all complete rank one sections of ARRAY', &
'     along the given dimension are shifted.', &
'', &
'  o  SHIFT : the number of elements to shift. A negative value shifts to the', &
'     right, a positive value to the left of the vector(s) being shifted.', &
'', &
'  o  BOUNDARY : the value to use to fill in the elements vacated by the shift.', &
'     If BOUNDARY is not present then the following are copied in depending on', &
'     the type of ARRAY.', &
'', &
'          Array Type     | Boundary Value', &
'          -----------------------------------------------------', &
'          Numeric        | 0, 0.0, or (0.0, 0.0)  of the type and kind of "array"', &
'          Logical        | .false.', &
'          Character(len) |  LEN blanks', &
'', &
'  These are the only types for which BOUNDARY may not be present. For these', &
'  types the kind is converted as neccessary to the kind of ARRAY.', &
'', &
'  o  DIM : DIM is in the range of', &
'', &
'     1 <= DIM <= n', &
'', &
'  where "N" is the rank of ARRAY. If DIM is omitted it is taken to be 1.', &
'', &
'RESULT', &
'  Returns an array of the same characteristics as the input with the specified', &
'  number of elements dropped off along the specified direction indicated,', &
'  backfilling the vacated elements with a value indicated by the BOUNDARY', &
'  value.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_eoshift', &
'      implicit none', &
'      integer, dimension(3,3) :: a', &
'      integer :: i', &
'', &
'          a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])', &
'          print ''(3i3)'', (a(i,:),i=1,3)', &
'', &
'          print *', &
'', &
'          ! shift it', &
'          a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)', &
'          print ''(3i3)'', (a(i,:),i=1,3)', &
'', &
'      end program demo_eoshift', &
'', &
'  Results:', &
'', &
'        >  1  4  7', &
'        >  2  5  8', &
'        >  3  6  9', &
'        >', &
'        >  4  7 -5', &
'        >  8 -5 -5', &
'        >  6  9 -5', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DSHIFTR(3), DSHIFTL(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             eoshift(3fortran)', &
'']

shortname="eoshift"
call process()


case('76','epsilon')

textblock=[character(len=256) :: &
'', &
'epsilon(3fortran)                                            epsilon(3fortran)', &
'', &
'NAME', &
'  EPSILON(3) - [NUMERIC MODEL] Epsilon function', &
'', &
'SYNOPSIS', &
'  result = epsilon(x)', &
'', &
'           real(kind=kind(x)) function epsilon(x)', &
'', &
'            real(kind=kind(x),intent(in)   :: x(..)', &
'', &
'CHARACTERISTICS', &
'  o  X shall be of type real. It may be a scalar or an array.', &
'', &
'  o  the result is a scalar of the same type and kind type parameter as X.', &
'', &
'DESCRIPTION', &
'  EPSILON(3) returns the floating point relative accuracy. It is the nearly', &
'  negligible number relative to 1 such that 1+ LITTLE_NUMBER is not equal to', &
'  1; or more precisely', &
'', &
'         real( 1.0, kind(x)) + epsilon(x) /=  real( 1.0, kind(x))', &
'', &
'  It may be thought of as the distance from 1.0 to the next largest floating', &
'  point number.', &
'', &
'  One use of EPSILON(3) is to select a delta value for algorithms that search', &
'  until the calculation is within delta of an estimate.', &
'', &
'  If delta is too small the algorithm might never halt, as a computation', &
'  summing values smaller than the decimal resolution of the data type does not', &
'  change.', &
'', &
'OPTIONS', &
'  o  X : The type shall be real.', &
'', &
'RESULT', &
'  The return value is of the same type as the argument.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_epsilon', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'      implicit none', &
'      real(kind=sp) :: x = 3.143', &
'      real(kind=dp) :: y = 2.33d0', &
'', &
'         ! so if x is of type real32, epsilon(x) has the value 2**-23', &
'         print *, epsilon(x)', &
'         ! note just the type and kind of x matter, not the value', &
'         print *, epsilon(huge(x))', &
'         print *, epsilon(tiny(x))', &
'', &
'         ! the value changes with the kind of the real value though', &
'         print *, epsilon(y)', &
'', &
'         ! adding and subtracting epsilon(x) changes x', &
'         write(*,*)x == x + epsilon(x)', &
'         write(*,*)x == x - epsilon(x)', &
'', &
'         ! these next two comparisons will be .true. !', &
'         write(*,*)x == x + epsilon(x) * 0.999999', &
'         write(*,*)x == x - epsilon(x) * 0.999999', &
'', &
'         ! you can calculate epsilon(1.0d0)', &
'         write(*,*)my_dp_eps()', &
'', &
'      contains', &
'', &
'         function my_dp_eps()', &
'         ! calculate the epsilon value of a machine the hard way', &
'         real(kind=dp) :: t', &
'         real(kind=dp) :: my_dp_eps', &
'', &
'            ! starting with a value of 1, keep dividing the value', &
'            ! by 2 until no change is detected. Note that with', &
'            ! infinite precision this would be an infinite loop,', &
'            ! but floating point values in Fortran have a defined', &
'            ! and limited precision.', &
'            my_dp_eps = 1.0d0', &
'            SET_ST: do', &
'               my_dp_eps = my_dp_eps/2.0d0', &
'               t = 1.0d0 + my_dp_eps', &
'               if (t <= 1.0d0) exit', &
'            enddo SET_ST', &
'            my_dp_eps = 2.0d0*my_dp_eps', &
'', &
'         end function my_dp_eps', &
'      end program demo_epsilon', &
'', &
'  Results:', &
'', &
'        1.1920929E-07', &
'        1.1920929E-07', &
'        1.1920929E-07', &
'        2.220446049250313E-016', &
'', &
'   F', &
'   F', &
'   T', &
'   T', &
'  2.220446049250313E-016', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             epsilon(3fortran)', &
'']

shortname="epsilon"
call process()


case('77','erf')

textblock=[character(len=256) :: &
'', &
'erf(3fortran)                                                    erf(3fortran)', &
'', &
'NAME', &
'  ERF(3) - [MATHEMATICS] Error function', &
'', &
'SYNOPSIS', &
'  result = erf(x)', &
'', &
'           elemental real(kind=KIND) function erf(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is of type real', &
'', &
'  o  The result is of the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  ERF(3) computes the error function of X, defined as', &
'', &
'  $$ \text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{-T^2} dt. $$', &
'', &
'OPTIONS', &
'  o  X : The type shall be real.', &
'', &
'RESULT', &
'  The return value is of type real, of the same kind as X and lies in the', &
'  range -1 <= ERF(x) <= 1 .', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_erf', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 0.17_real64', &
'          write(*,*)x, erf(x)', &
'      end program demo_erf', &
'', &
'  Results:', &
'', &
'           0.17000000000000001       0.18999246120180879', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  ERFC(3), ERF_SCALED(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:error function', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022                 erf(3fortran)', &
'']

shortname="erf"
call process()


case('78','erfc')

textblock=[character(len=256) :: &
'', &
'erfc(3fortran)                                                  erfc(3fortran)', &
'', &
'NAME', &
'  ERFC(3) - [MATHEMATICS] Complementary error function', &
'', &
'SYNOPSIS', &
'  result = erfc(x)', &
'', &
'           elemental real(kind=KIND) function erfc(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is of type real and any valid kind', &
'', &
'  o  KIND is any value valid for type real', &
'', &
'  o  the result has the same characteristics as X', &
'', &
'DESCRIPTION', &
'  ERFC(3) computes the complementary error function of X. Simply put this is', &
'  equivalent to 1 - ERF(X), but ERFC is provided because of the extreme loss', &
'  of relative accuracy if ERF(X) is called for large X and the result is', &
'  subtracted from 1.', &
'', &
'  ERFC(X) is defined as', &
'', &
'  $$ \text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}}', &
'  \int_x^{\infty} e^{-t^2} dt. $$', &
'', &
'OPTIONS', &
'  o  X : The type shall be real.', &
'', &
'RESULT', &
'  The return value is of type real and of the same kind as X. It lies in the', &
'  range', &
'', &
'    0 \<= **erfc**(x) \<= 2.', &
'', &
'  and is a processor-dependent approximation to the complementary error', &
'  function of X ( **1-erf(x) ).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_erfc', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'       & real_kinds, real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 0.17_real64', &
'         write(*,''(*(g0))'')''X='',x, '' ERFC(X)='',erfc(x)', &
'         write(*,''(*(g0))'')''equivalently 1-ERF(X)='',1-erf(x)', &
'      end program demo_erfc', &
'', &
'  Results:', &
'', &
'       > X=.1700000000000000 ERFC(X)=.8100075387981912', &
'       > equivalently 1-ERF(X)=.8100075387981912', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  ERF(3) ERF_SCALED(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:error function', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                erfc(3fortran)', &
'']

shortname="erfc"
call process()


case('79','erfc_scaled')

textblock=[character(len=256) :: &
'', &
'erfc_scaled(3fortran)                                    erfc_scaled(3fortran)', &
'', &
'NAME', &
'  ERFC_SCALED(3) - [MATHEMATICS] Scaled complementary error function', &
'', &
'SYNOPSIS', &
'  result = erfc_scaled(x)', &
'', &
'           elemental real(kind=KIND) function erfc_scaled(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is of type real of any valid kind', &
'', &
'  o  KIND is any kind valid for a real type', &
'', &
'  o  the result has the same characteristics as X', &
'', &
'DESCRIPTION', &
'  ERFC_SCALED(3) computes the exponentially-scaled complementary error', &
'  function of X:', &
'', &
'  $$ e^{x^2} \frac{2}{\sqrt{\pi}} \int_{x}^{\infty} e^{-t^2} dt. $$', &
'', &
'  erfc_scaled(x)=exp(x*x)erfc(x)', &
'', &
'  NOTE1', &
'', &
'  The complementary error function is asymptotic to exp(-X2)/(X/PI). As such', &
'  it underflows at approximately X >= 9 when using ISO/IEC/IEEE 60559:2011', &
'  single precision arithmetic. The exponentially-scaled complementary error', &
'  function is asymptotic to 1/(X PI). As such it does not underflow until X >', &
'  HUGE (X)/PI.', &
'', &
'OPTIONS', &
'  o  X the value to apply the ERFC function to', &
'', &
'RESULT', &
'  The approximation to the exponentially-scaled complementary error function', &
'  of X', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_erfc_scaled', &
'      implicit none', &
'      real(kind(0.0d0)) :: x = 0.17d0', &
'         x = erfc_scaled(x)', &
'         print *, x', &
'      end program demo_erfc_scaled', &
'', &
'  Results:', &
'', &
'       >   0.833758302149981', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  ERF(3), EXP(3), ERFC(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022         erfc_scaled(3fortran)', &
'']

shortname="erfc_scaled"
call process()


case('80','event_query')

textblock=[character(len=256) :: &
'', &
'event_query(3fortran)                                    event_query(3fortran)', &
'', &
'NAME', &
'  EVENT_QUERY(3) - [COLLECTIVE] Query whether a coarray event has occurred', &
'', &
'SYNOPSIS', &
'  call event_query(event, count [,stat] )', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  EVENT_QUERY(3) assigns the number of events to COUNT which have been posted', &
'  to the EVENT variable and not yet been removed by calling EVENT_WAIT. When', &
'  STAT is present and the invocation was successful, it is assigned the value', &
'  0. If it is present and the invocation has failed, it is assigned a positive', &
'  value and COUNT is assigned the value -1.', &
'', &
'OPTIONS', &
'  o  EVENT : (intent(in)) Scalar of type event_type, defined in', &
'     iso_fortran_env; shall not be coindexed.', &
'', &
'  o  COUNT : (intent(out))Scalar integer with at least the precision of', &
'     default integer.', &
'', &
'  o  STAT : (OPTIONAL) Scalar default-kind integer variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_event_query', &
'      use iso_fortran_env', &
'      implicit none', &
'      type(event_type) :: event_value_has_been_set[*]', &
'      integer :: cnt', &
'         if (this_image() == 1) then', &
'            call event_query(event_value_has_been_set, cnt)', &
'            if (cnt > 0) write(*,*) "Value has been set"', &
'         elseif (this_image() == 2) then', &
'            event post(event_value_has_been_set[1])', &
'         endif', &
'      end program demo_event_query', &
'', &
'STANDARD', &
'  TS 18508', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022         event_query(3fortran)', &
'']

shortname="event_query"
call process()


case('81','execute_command_line')

textblock=[character(len=256) :: &
'', &
'execute_command_line(3fortran)                  execute_command_line(3fortran)', &
'', &
'NAME', &
'  EXECUTE_COMMAND_LINE(3) - [SYSTEM:PROCESSES] Execute a shell command', &
'', &
'SYNOPSIS', &
'  call execute_command_line( & & command [,wait] [,exitstat] [,cmdstat]', &
'  [,cmdmsg] )', &
'', &
'           subroutine execute_command_line(command,wait,exitstat,cmdstat,cmdmsg)', &
'', &
'            character(len=*),intent(in)             :: command', &
'            logical,intent(in),optional             :: wait', &
'            integer,intent(inout),optional          :: exitstat', &
'            integer,intent(inout),optional          :: cmdstat', &
'            character(len=*),intent(inout),optional :: cmdmsg', &
'', &
'CHARACTERISTICS', &
'  o  COMMAND is a default character scalar', &
'', &
'  o  WAIT is a default logical scalar. If WAIT is present with the', &
'', &
'  o  EXITSTAT is an integer of the default kind. It must be of a kind with at', &
'     least a decimal exponent range of 9.', &
'', &
'  o  CMDSTAT is an integer of default kind The kind of the variable must', &
'     support at least a decimal exponent range of four.', &
'', &
'  o  CMDMSG is a character scalar of the default kind.', &
'', &
'DESCRIPTION', &
'  For EXECUTE_COMMAND_LINE(3) the COMMAND argument is passed to the shell and', &
'  executed. (The shell is generally SH(1) on Unix systems, and cmd.exe on', &
'  Windows.) If WAIT is present and has the value .false., the execution of the', &
'  command is asynchronous if the system supports it; otherwise, the command is', &
'  executed synchronously.', &
'', &
'  The three last arguments allow the user to get status information. After', &
'  synchronous execution, EXITSTAT contains the integer exit code of the', &
'  command, as returned by SYSTEM. CMDSTAT is set to zero if the command line', &
'  was executed (whatever its exit status was). CMDMSG is assigned an error', &
'  message if an error has occurred.', &
'', &
'  Note that the system call need not be thread-safe. It is the responsibility', &
'  of the user to ensure that the system is not called concurrently if', &
'  required.', &
'', &
'  When the command is executed synchronously, EXECUTE_COMMAND_LINE returns', &
'  after the command line has completed execution. Otherwise,', &
'  EXECUTE_COMMAND_LINE returns without waiting.', &
'', &
'  Because this intrinsic is making a system call, it is very system dependent.', &
'  Its behavior with respect to signaling is processor dependent. In', &
'  particular, on POSIX-compliant systems, the SIGINT and SIGQUIT signals will', &
'  be ignored, and the SIGCHLD will be blocked. As such, if the parent process', &
'  is terminated, the child process might not be terminated alongside.', &
'', &
'OPTIONS', &
'  o  COMMAND : the command line to be executed. The interpretation is', &
'     programming-environment dependent.', &
'', &
'  o  WAIT : If WAIT is present with the value .false., and the processor', &
'     supports asynchronous execution of the command, the command is executed', &
'     asynchronously; otherwise it is executed synchronously.', &
'', &
'     When the command is executed synchronously, EXECUTE_COMMAND_LINE(3)', &
'     returns after the command line has completed execution. Otherwise,', &
'     EXECUTE_COMMAND_LINE(3) returns without waiting.', &
'', &
'  o  EXITSTAT : If the command is executed synchronously, it is assigned the', &
'     value of the processor-dependent exit status. Otherwise, the value of', &
'     EXITSTAT is unchanged.', &
'', &
'     If the command is executed synchronously, it is assigned the value of the', &
'     processor-dependent exit status. Otherwise, the value of EXITSTAT is', &
'     unchanged.', &
'', &
'  o  CMDSTAT : If an error condition occurs and CMDSTAT is not present, error', &
'     termination of execution of the image is initiated.', &
'', &
'     It is assigned the value -1 if the processor does not support command', &
'     line execution, a processor-dependent positive value if an error', &
'     condition occurs, or the value -2 if no error condition occurs but WAIT', &
'     is present with the value false and the processor does not support', &
'     asynchronous execution. Otherwise it is assigned the value 0.', &
'', &
'  o  CMDMSG : If an error condition occurs, it is assigned a processor-', &
'     dependent explanatory message. Otherwise, it is unchanged.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_exec', &
'      implicit none', &
'         integer :: i', &
'', &
'         call execute_command_line("external_prog.exe", exitstat=i)', &
'         print *, "Exit status of external_prog.exe was ", i', &
'', &
'         call execute_command_line("reindex_files.exe", wait=.false.)', &
'         print *, "Now reindexing files in the background"', &
'      end program demo_exec', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  GET_ENVIRONMENT_VARIABLE(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022execute_command_line(3fortran)', &
'']

shortname="execute_command_line"
call process()


case('82','exp')

textblock=[character(len=256) :: &
'', &
'exp(3fortran)                                                    exp(3fortran)', &
'', &
'NAME', &
'  EXP(3) - [MATHEMATICS] Base-e exponential function', &
'', &
'SYNOPSIS', &
'  result = exp(x)', &
'', &
'           elemental TYPE(kind=KIND) function exp(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be real or complex of any kind.', &
'', &
'  o  The return value has the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  EXP(3) returns the value of e (the base of natural logarithms) raised to the', &
'  power of X.', &
'', &
'  "e" is also known as Euler''s constant.', &
'', &
'  If X is of type complex, its imaginary part is regarded as a value in', &
'  radians such that if (see Euler''s formula):', &
'', &
'          cx=(re,im)', &
'', &
'  then', &
'', &
'          exp(cx) = exp(re) * cmplx(cos(im),sin(im),kind=kind(cx))', &
'', &
'  Since EXP(3) is the inverse function of LOG(3) the maximum valid magnitude', &
'  of the real component of X is LOG(HUGE(X)).', &
'', &
'OPTIONS', &
'  o  X : The type shall be real or complex.', &
'', &
'RESULT', &
'  The value of the result is E**X where E is Euler''s constant.', &
'', &
'  If X is of type complex, its imaginary part is regarded as a value in', &
'  radians.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_exp', &
'      implicit none', &
'      real :: x, re, im', &
'      complex :: cx', &
'', &
'         x = 1.0', &
'         write(*,*)"Euler''s constant is approximately",exp(x)', &
'', &
'         !! complex values', &
'         ! given', &
'         re=3.0', &
'         im=4.0', &
'         cx=cmplx(re,im)', &
'', &
'         ! complex results from complex arguments are Related to Euler''s formula', &
'         write(*,*)''given the complex value '',cx', &
'         write(*,*)''exp(x) is'',exp(cx)', &
'         write(*,*)''is the same as'',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))', &
'', &
'         ! exp(3) is the inverse function of log(3) so', &
'         ! the real component of the input must be less than or equal to', &
'         write(*,*)''maximum real component'',log(huge(0.0))', &
'         ! or for double precision', &
'         write(*,*)''maximum doubleprecision component'',log(huge(0.0d0))', &
'', &
'         ! but since the imaginary component is passed to the cos(3) and sin(3)', &
'         ! functions the imaginary component can be any real value', &
'', &
'      end program demo_exp', &
'', &
'  Results:', &
'', &
'       Euler''s constant is approximately   2.718282', &
'       given the complex value  (3.000000,4.000000)', &
'       exp(x) is (-13.12878,-15.20078)', &
'       is the same as (-13.12878,-15.20078)', &
'       maximum real component   88.72284', &
'       maximum doubleprecision component   709.782712893384', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  LOG(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:Exponential function', &
'', &
'  o  Wikipedia:Euler''s formula', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 exp(3fortran)', &
'']

shortname="exp"
call process()


case('83','exponent')

textblock=[character(len=256) :: &
'', &
'exponent(3fortran)                                          exponent(3fortran)', &
'', &
'NAME', &
'  EXPONENT(3) - [MODEL_COMPONENTS] Exponent of floating-point number', &
'', &
'SYNOPSIS', &
'  result = exponent(x)', &
'', &
'           elemental integer function exponent(x)', &
'', &
'            real(kind=**),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X shall be of type real of any valid kind', &
'', &
'  o  the result is a default integer type', &
'', &
'DESCRIPTION', &
'  EXPONENT(3) returns the value of the exponent part of X, provided the', &
'  exponent is within the range of default integers.', &
'', &
'OPTIONS', &
'  o  X : the value to query the exponent of', &
'', &
'RESULT', &
'  EXPONENT(3) returns the value of the exponent part of X', &
'', &
'  If X is zero the value returned is zero.', &
'', &
'  If X is an IEEE infinity or NaN, the result has the value HUGE(0).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_exponent', &
'      implicit none', &
'      real :: x = 1.0', &
'      integer :: i', &
'         i = exponent(x)', &
'         print *, i', &
'         print *, exponent(0.0)', &
'         print *, exponent([10.0,100.0,1000.0,-10000.0])', &
'         print *, 2**[10.0,100.0,1000.0,-10000.0]', &
'         print *, exponent(huge(0.0))', &
'         print *, exponent(tiny(0.0))', &
'      end program demo_exponent', &
'', &
'  Results:', &
'', &
'       >            4           7          10          14', &
'       >          128', &
'       >         -125', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), FRACTION(3), HUGE(3), MAXEXPONENT(3), MINEXPONENT(3),', &
'  NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022            exponent(3fortran)', &
'']

shortname="exponent"
call process()


case('84','extends_type_of')

textblock=[character(len=256) :: &
'', &
'extends_type_of(3fortran)                            extends_type_of(3fortran)', &
'', &
'NAME', &
'  EXTENDS_TYPE_OF(3) - [STATE:INQUIRY] Determine if the dynamic type of A is', &
'  an extension of the dynamic type of MOLD.', &
'', &
'SYNOPSIS', &
'  result = extends_type_of(a, mold)', &
'', &
'           logical extends_type_of(a, mold)', &
'', &
'            type(TYPE(kind=KIND),intent(in) :: a', &
'            type(TYPE(kind=KIND),intent(in) :: mold', &
'', &
'CHARACTERISTICS', &
'  -A shall be an object or pointer to an extensible declared type, or', &
'  unlimited polymorphic. If it is a polymorphic pointer, it shall not have an', &
'  undefined association status. -MOLE shall be an object or pointer to an', &
'  extensible declared type or unlimited polymorphic. If it is a polymorphic', &
'  pointer, it shall not have an undefined association status.', &
'', &
'  o  the result is a scalar default logical type.', &
'', &
'DESCRIPTION', &
'  EXTENDS_TYPE_OF(3) is .true. if and only if the dynamic type of A is or', &
'  could be (for unlimited polymorphic) an extension of the dynamic type of', &
'  MOLD.', &
'', &
'  NOTE1', &
'', &
'  The dynamic type of a disassociated pointer or unallocated allocatable', &
'  variable is its declared type.', &
'', &
'  NOTE2', &
'', &
'  The test performed by EXTENDS_TYPE_OF is not the same as the test performed', &
'  by the type guard CLASS IS. The test performed by EXTENDS_TYPE_OF does not', &
'  consider kind type parameters.', &
'', &
'OPTIONS', &
'  o  A : be an object of extensible declared type or unlimited polymorphic. If', &
'     it is a polymorphic pointer, it shall not have an undefined association', &
'     status.', &
'', &
'  o  MOLD : be an object of extensible declared type or unlimited polymorphic.', &
'     If it is a polymorphic pointer, it shall not have an undefined', &
'     association status.', &
'', &
'RESULT', &
'  If MOLD is unlimited polymorphic and is either a disassociated pointer or', &
'  unallocated allocatable variable, the result is true.', &
'', &
'  Otherwise if A is unlimited polymorphic and is either a disassociated', &
'  pointer or unallocated allocatable variable, the result is false.', &
'', &
'  Otherwise the result is true if and only if the dynamic type of A', &
'', &
'  if the dynamic type of A or MOLD is extensible, the result is true if and', &
'  only if the dynamic type of A is an extension type of the dynamic type of', &
'  MOLD; otherwise the result is processor dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'        ! program demo_extends_type_of', &
'        module M_demo_extends_type_of', &
'        implicit none', &
'        private', &
'', &
'        type nothing', &
'        end type nothing', &
'', &
'        type, extends(nothing) :: dot', &
'          real :: x=0', &
'          real :: y=0', &
'        end type dot', &
'', &
'        type, extends(dot) :: point', &
'          real :: z=0', &
'        end type point', &
'', &
'        type something_else', &
'        end type something_else', &
'', &
'        public :: nothing', &
'        public :: dot', &
'        public :: point', &
'        public :: something_else', &
'', &
'        end module M_demo_extends_type_of', &
'', &
'        program demo_extends_type_of', &
'        use M_demo_extends_type_of, only : nothing, dot, point, something_else', &
'        implicit none', &
'        type(nothing) :: grandpa', &
'        type(dot) :: dad', &
'        type(point) :: me', &
'        type(something_else) :: alien', &
'', &
'         write(*,*)''these should all be true''', &
'         write(*,*)extends_type_of(me,grandpa),''I am descended from Grandpa''', &
'         write(*,*)extends_type_of(dad,grandpa),''Dad is descended from Grandpa''', &
'         write(*,*)extends_type_of(me,dad),''Dad is my ancestor''', &
'', &
'         write(*,*)''is an object an extension of itself?''', &
'         write(*,*)extends_type_of(grandpa,grandpa) ,''self-propagating!''', &
'         write(*,*)extends_type_of(dad,dad) ,''clone!''', &
'', &
'         write(*,*)'' you did not father your grandfather''', &
'         write(*,*)extends_type_of(grandpa,dad),''no paradox here''', &
'', &
'         write(*,*)extends_type_of(dad,me),''no paradox here''', &
'         write(*,*)extends_type_of(grandpa,me),''no relation whatsoever''', &
'         write(*,*)extends_type_of(grandpa,alien),''no relation''', &
'         write(*,*)extends_type_of(me,alien),''not what everyone thinks''', &
'', &
'         call pointers()', &
'         contains', &
'', &
'         subroutine pointers()', &
'         ! Given the declarations and assignments', &
'         type t1', &
'         real c', &
'         end type', &
'         type, extends(t1) :: t2', &
'         end type', &
'         class(t1), pointer :: p, q', &
'            allocate (p)', &
'            allocate (t2 :: q)', &
'            ! the result of EXTENDS_TYPE_OF (P, Q) will be false, and the result', &
'            ! of EXTENDS_TYPE_OF (Q, P) will be true.', &
'            write(*,*)''(P,Q)'',extends_type_of(p,q),"mind your P''s and Q''s"', &
'            write(*,*)''(Q,P)'',extends_type_of(q,p)', &
'         end subroutine pointers', &
'', &
'        end program demo_extends_type_of', &
'', &
'  Results:', &
'', &
'          these should all be true', &
'          T I am descended from Grandpa', &
'          T Dad is descended from Grandpa', &
'          T Dad is my ancestor', &
'          is an object an extension of itself?', &
'          T self-propagating!', &
'          T clone!', &
'           you did not father your grandfather', &
'          F no paradox here', &
'          F no paradox here', &
'          F no relation whatsoever', &
'          F no relation', &
'          F not what everyone thinks', &
'          (P,Q) F mind your P''s and Q''s', &
'          (Q,P) T', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  SAME_TYPE_AS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022     extends_type_of(3fortran)', &
'']

shortname="extends_type_of"
call process()


case('85','findloc')

textblock=[character(len=256) :: &
'', &
'findloc(3fortran)                                            findloc(3fortran)', &
'', &
'NAME', &
'  FINDLOC(3) - [ARRAY:LOCATION] Location of first element of ARRAY identified', &
'  by MASK along dimension DIM matching a target value', &
'', &
'SYNOPSIS', &
'  result = findloc (array, value, dim [,mask] [,kind] [,back]) | findloc', &
'  (array, value [,mask] [,kind] [,back])', &
'', &
'    function findloc (array, value, dim, mask, kind, back)', &
'', &
'              type TYPE(kind=KIND),intent(in)      :: array(..)', &
'              type TYPE(kind=KIND),intent(in)      :: value', &
'              integer(kind=**),intent(in),optional :: dim', &
'              logical(kind=**),intent(in),optional :: mask(..)', &
'              integer(kind=**),intent(in),optional :: kind', &
'              logical(kind=**),intent(in),optional :: back', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY is an array of any intrinsic type.', &
'', &
'  o  VALUE shall be scalar but in type conformance with ARRAY, as specified', &
'     for the operator == or the operator .EQV..', &
'', &
'  o  DIM an integer corresponding to a dimension of ARRAY. The corresponding', &
'     actual argument shall not be an optional dummy argument.', &
'', &
'  o  MASK is logical and shall be conformable with ARRAY.', &
'', &
'  o  KIND a scalar integer initialization expression (ie. a constant)', &
'', &
'  o  BACK a logical scalar.', &
'', &
'  o  the result is integer of default kind or kind KIND if the KIND argument', &
'     is present. If DIM does not appear, the result is an array of rank one', &
'     and of size equal to the rank of ARRAY; otherwise, the result is an array', &
'     of the same rank and shape as ARRAY reduced by the dimension DIM.', &
'', &
'  NOTE: a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  FINDLOC(3) returns the location of the first element of ARRAY identified by', &
'  MASK along dimension DIM having a value equal to VALUE.', &
'', &
'  If both ARRAY and VALUE are of type logical, the comparison is performed', &
'  with the .EQV. operator; otherwise, the comparison is performed with the ==', &
'  operator. If the value of the comparison is .true., that element of ARRAY', &
'  matches VALUE.', &
'', &
'  If only one element matches VALUE, that element''s subscripts are returned.', &
'  Otherwise, if more than one element matches VALUE and BACK is absent or', &
'  present with the value .false., the element whose subscripts are returned is', &
'  the first such element, taken in array element order. If BACK is present', &
'  with the value .true., the element whose subscripts are returned is the last', &
'  such element, taken in array element order.', &
'', &
'OPTIONS', &
'  o  ARRAY : shall be an array of intrinsic type.', &
'', &
'  o  VALUE : shall be scalar and in type conformance with ARRAY.', &
'', &
'  o  DIM : shall be an integer scalar with a value in the range 1 <= DIM <= n,', &
'     where n is the rank of ARRAY. The corresponding actual argument shall not', &
'     be an optional dummy argument.', &
'', &
'  o  MASK : (optional) shall be of type logical and shall be conformable with', &
'     ARRAY.', &
'', &
'  o  KIND : (optional) shall be a scalar integer initialization expression.', &
'', &
'  o  BACK : (optional) shall be a logical scalar.', &
'', &
'RESULT', &
'  KIND is present, the kind type parameter is that specified by the value of', &
'  KIND; otherwise the kind type parameter is that of default integer type. If', &
'  DIM does not appear, the result is an array of rank one and of size equal to', &
'  the rank of ARRAY; otherwise, the result is of rank n - 1 and shape', &
'', &
'         [d1, d2, . . ., dDIM-1, dDIM+1, . . ., dn ]', &
'', &
'  where', &
'', &
'         [d1, d2, . . ., dn ]', &
'', &
'  is the shape of ARRAY.', &
'', &
'RESULT', &
'  o  CASE (I): The result of FINDLOC (ARRAY, VALUE) is a rank-one array whose', &
'     element values are the values of the subscripts of an element of ARRAY', &
'     whose value matches VALUE. If there is such a value, the ith subscript', &
'     returned lies in the range 1 to ei, where ei is the extent of the ith', &
'     dimension of ARRAY. If no elements match VALUE or ARRAY has size zero,', &
'     all elements of the result are zero.', &
'', &
'  o  CASE (II): the result of FINDLOC (ARRAY, VALUE, MASK = MASK) is a rank-', &
'     one array whose element values are the values of the subscripts of an', &
'     element of ARRAY, corresponding to a true element of MASK, whose value', &
'     matches VALUE. If there is such a value, the ith subscript returned lies', &
'     in the range 1 to ei, where ei is the extent of the ith dimension of', &
'     ARRAY. If no elements match VALUE, ARRAY has size zero, or every element', &
'     of MASK has the value false, all elements of the result are zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_findloc', &
'      logical,parameter :: T=.true., F=.false.', &
'      integer,allocatable :: ibox(:,:)', &
'      logical,allocatable :: mask(:,:)', &
'        ! basics', &
'         ! the first element matching the value is returned AS AN ARRAY', &
'         call printi(''== 6'',findloc ([2, 6, 4, 6], value = 6))', &
'         call printi(''== 6'',findloc ([2, 6, 4, 6], value = 6,back=.true.))', &
'         ! the first element matching the value is returned AS A SCALAR', &
'         call printi(''== 6'',findloc ([2, 6, 4, 6], value = 6,dim=1))', &
'         call printi(''== 6'',findloc ([2, 6, 4, 6], value = 6,back=.true.,dim=1))', &
'', &
'         ibox=reshape([ 0,-5,  7, 7, &', &
'                        3, 4, -1, 2, &', &
'                        1, 5,  6, 7] ,shape=[3,4],order=[2,1])', &
'', &
'         mask=reshape([ T, T, F, T, &', &
'                        T, T, F, T, &', &
'                        T, T, F, T] ,shape=[3,4],order=[2,1])', &
'', &
'         call printi(''array is'', ibox )', &
'         call printl(''mask  is'', mask )', &
'         print *, ''so for == 7 and back=.false.''', &
'         call printi(''so for == 7 the address of the element is'', &', &
'                 & findloc (ibox, 7, mask = mask) )', &
'         print *, ''so for == 7 and back=.true.''', &
'         call printi(''so for == 7 the address of the element is'', &', &
'                 & findloc (ibox, 7, mask = mask, back=.true.) )', &
'', &
'         print *,''This is independent of declared lower bounds for the array''', &
'', &
'         print *, '' using dim=N''', &
'         ibox=reshape([ 1,  2, -9,  &', &
'                        2,  2,  6 ] ,shape=[2,3],order=[2,1])', &
'', &
'         call printi(''array is'', ibox )', &
'         ! has the value [2, 1, 0] and', &
'         call printi('''',findloc (ibox, value = 2, dim = 1) )', &
'         ! has the value [2, 1].', &
'         call printi('''',findloc (ibox, value = 2, dim = 2) )', &
'      contains', &
'      ! GENERIC ROUTINES TO PRINT MATRICES', &
'      subroutine printl(title,a)', &
'      implicit none', &
'      !@(#) print small 2d logical scalar, vector, matrix in row-column format', &
'      character(len=*),intent(in)  :: title', &
'      logical,intent(in)           :: a(..)', &
'', &
'      character(len=*),parameter   :: row=''(" > [ ",*(l1:,","))''', &
'      character(len=*),parameter   :: all=''(" ",*(g0,1x))''', &
'      logical,allocatable          :: b(:,:)', &
'      integer                      :: i', &
'         write(*,all,advance=''no'')trim(title)', &
'         ! copy everything to a matrix to keep code simple', &
'         select rank(a)', &
'         rank (0); write(*,''(a)'')'' (a scalar)''; b=reshape([a],[1,1])', &
'         rank (1); write(*,''(a)'')'' (a vector)''; b=reshape(a,[size(a),1])', &
'         rank (2); write(*,''(a)'')'' (a matrix)''; b=a', &
'         rank default; stop ''*printl* unexpected rank''', &
'         end select', &
'         do i=1,size(b,dim=1)', &
'            write(*,fmt=row,advance=''no'')b(i,:)', &
'            write(*,''(" ]")'')', &
'         enddo', &
'         write(*,all) ''>shape='',shape(a),'',rank='',rank(a),'',size='',size(a)', &
'         write(*,*)', &
'      end subroutine printl', &
'', &
'      subroutine printi(title,a)', &
'      implicit none', &
'      !@(#) print small 2d integer scalar, vector, matrix in row-column format', &
'      character(len=*),intent(in)  :: title', &
'      integer,intent(in)           :: a(..)', &
'      character(len=*),parameter   :: all=''(" ",*(g0,1x))''', &
'      character(len=20)            :: row', &
'      integer,allocatable          :: b(:,:)', &
'      integer                      :: i', &
'         write(*,all,advance=''no'')trim(title)', &
'         ! copy everything to a matrix to keep code simple', &
'         select rank(a)', &
'         rank (0); write(*,''(a)'')'' (a scalar)''; b=reshape([a],[1,1])', &
'         rank (1); write(*,''(a)'')'' (a vector)''; b=reshape(a,[size(a),1])', &
'         rank (2); write(*,''(a)'')'' (a matrix)''; b=a', &
'         rank default; stop ''*printi* unexpected rank''', &
'         end select', &
'         ! find how many characters to use for integers', &
'         write(row,''(i0)'')ceiling(log10(real(maxval(abs(b)))))+2', &
'         ! use this format to write a row', &
'         row=''(" > [",*(i''//trim(row)//'':,","))''', &
'         do i=1,size(b,dim=1)', &
'            write(*,fmt=row,advance=''no'')b(i,:)', &
'            write(*,''(" ]")'')', &
'         enddo', &
'         write(*,all) ''>shape='',shape(a),'',rank='',rank(a),'',size='',size(a)', &
'         write(*,*)', &
'      end subroutine printi', &
'      end program demo_findloc', &
'', &
'  Results:', &
'', &
'       >  == 6  (a vector)', &
'       >  > [  2 ]', &
'       >  >shape= 1 ,rank= 1 ,size= 1', &
'       >', &
'       >  == 6  (a vector)', &
'       >  > [  4 ]', &
'       >  >shape= 1 ,rank= 1 ,size= 1', &
'       >', &
'       >  == 6  (a scalar)', &
'       >  > [  2 ]', &
'       >  >shape= ,rank= 0 ,size= 1', &
'       >', &
'       >  == 6  (a scalar)', &
'       >  > [  4 ]', &
'       >  >shape= ,rank= 0 ,size= 1', &
'       >', &
'       >  array is  (a matrix)', &
'       >  > [  0, -5,  7,  7 ]', &
'       >  > [  3,  4, -1,  2 ]', &
'       >  > [  1,  5,  6,  7 ]', &
'       >  >shape= 3 4 ,rank= 2 ,size= 12', &
'       >', &
'       >  mask  is  (a matrix)', &
'       >  > [ T,T,F,T ]', &
'       >  > [ T,T,F,T ]', &
'       >  > [ T,T,F,T ]', &
'       >  >shape= 3 4 ,rank= 2 ,size= 12', &
'       >', &
'       >  so for == 7 and back=.false.', &
'       >  so for == 7 the address of the element is  (a vector)', &
'       >  > [  1 ]', &
'       >  > [  4 ]', &
'       >  >shape= 2 ,rank= 1 ,size= 2', &
'       >', &
'       >  so for == 7 and back=.true.', &
'       >  so for == 7 the address of the element is  (a vector)', &
'       >  > [  3 ]', &
'       >  > [  4 ]', &
'       >  >shape= 2 ,rank= 1 ,size= 2', &
'       >', &
'       >  This is independent of declared lower bounds for the array', &
'       >   using dim=N', &
'       >  array is  (a matrix)', &
'       >  > [  1,  2, -9 ]', &
'       >  > [  2,  2,  6 ]', &
'       >  >shape= 2 3 ,rank= 2 ,size= 6', &
'       >', &
'       >    (a vector)', &
'       >  > [  2 ]', &
'       >  > [  1 ]', &
'       >  > [  0 ]', &
'       >  >shape= 3 ,rank= 1 ,size= 3', &
'       >', &
'       >    (a vector)', &
'       >  > [  2 ]', &
'       >  > [  1 ]', &
'       >  >shape= 2 ,rank= 1 ,size= 2', &
'       >', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  MAXLOC(3) - Location of the maximum value within an array', &
'', &
'  o  MINLOC(3) - Location of the minimum value within an array', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             findloc(3fortran)', &
'']

shortname="findloc"
call process()


case('86','floor')

textblock=[character(len=256) :: &
'', &
'floor(3fortran)                                                floor(3fortran)', &
'', &
'NAME', &
'  FLOOR(3) - [NUMERIC] Function to return largest integral value not greater', &
'  than argument', &
'', &
'SYNOPSIS', &
'  result = floor(a [,kind])', &
'', &
'           elemental integer(kind=KIND) function floor( a ,kind )', &
'', &
'            real(kind=**),intent(in) :: a', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  A is a real of any kind', &
'', &
'  o  KIND is any valid value for type integer.', &
'', &
'  o  the result is an integer of the specified or default kind', &
'', &
'DESCRIPTION', &
'  FLOOR(3) returns the greatest integer less than or equal to A.', &
'', &
'  In other words, it picks the whole number at or to the left of the value on', &
'  the number line.', &
'', &
'  This means care has to be taken that the magnitude of the real value A does', &
'  not exceed the range of the output value, as the range of values supported', &
'  by real values is typically larger than the range for integers.', &
'', &
'OPTIONS', &
'  o  A : The value to operate on. Valid values are restricted by the size of', &
'     the returned integer kind to the range -HUGE(INT(A,KIND=KIND))-1 to', &
'     HUGE(INT(A),KIND=KIND).', &
'', &
'  o  KIND : A scalar integer constant initialization expression indicating the', &
'     kind parameter of the result.', &
'', &
'RESULT', &
'  The return value is of type integer(kind) if KIND is present and of default-', &
'  kind integer otherwise.', &
'', &
'  The result is undefined if it cannot be represented in the specified integer', &
'  type.', &
'', &
'  If in range for the kind of the result the result is the whole number at or', &
'  to the left of the input value on the number line.', &
'', &
'  If A is positive the result is the value with the fractional part removed.', &
'', &
'  If A is negative, it is the whole number at or to the left of the input', &
'  value.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_floor', &
'      implicit none', &
'      real :: x = 63.29', &
'      real :: y = -63.59', &
'          print *, x, floor(x)', &
'          print *, y, floor(y)', &
'         ! elemental', &
'         print *,floor([ &', &
'         &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'         &  0.0,   &', &
'         &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'         ! note even a small deviation from the whole number changes the result', &
'         print *,      [2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)]', &
'         print *,floor([2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)])', &
'', &
'         ! A=Nan, Infinity or  <huge(0_KIND)-1 < A > huge(0_KIND) is undefined', &
'      end program demo_floor', &
'', &
'  Results:', &
'', &
'       >     63.29000             63', &
'       >    -63.59000            -64', &
'       >            -3         -3         -3         -2         -2         -1', &
'       >            -1          0          0          1          1          2', &
'       >             2          2          2', &
'       >     2.000000      2.000000      2.000000', &
'       >             2          1          1', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  CEILING(3), NINT(3), AINT(3), ANINT(3), INT(3), SELECTED_INT_KIND(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               floor(3fortran)', &
'']

shortname="floor"
call process()


case('87','fraction')

textblock=[character(len=256) :: &
'', &
'fraction(3fortran)                                          fraction(3fortran)', &
'', &
'NAME', &
'  FRACTION(3) - [MODEL_COMPONENTS] Fractional part of the model representation', &
'', &
'SYNOPSIS', &
'  result = fraction(x)', &
'', &
'           elemental real(kind=KIND) function fraction(x)', &
'', &
'            real(kind=KIND),intent(in) :: fraction', &
'', &
'CHARACTERISTICS', &
'  o  X is of type real', &
'', &
'  o  The result has the same characteristics as the argument.', &
'', &
'DESCRIPTION', &
'  FRACTION(3) returns the fractional part of the model representation of X.', &
'', &
'OPTIONS', &
'  o  X : The value to interrogate', &
'', &
'RESULT', &
'  The fractional part of the model representation of X is returned; it is X *', &
'  RADIX(X)**(-EXPONENT(X)).', &
'', &
'  If X has the value zero, the result is zero.', &
'', &
'  If X is an IEEE NaN, the result is that NaN.', &
'', &
'  If X is an IEEE infinity, the result is an IEEE NaN.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_fraction', &
'      implicit none', &
'      real :: x', &
'         x = 178.1387e-4', &
'         print *, fraction(x), x * radix(x)**(-exponent(x))', &
'      end program demo_fraction', &
'', &
'  Results:', &
'', &
'           0.5700439      0.5700439', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), HUGE(3), MAXEXPONENT(3), MINEXPONENT(3),', &
'  NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022            fraction(3fortran)', &
'']

shortname="fraction"
call process()


case('88','gamma')

textblock=[character(len=256) :: &
'', &
'gamma(3fortran)                                                gamma(3fortran)', &
'', &
'NAME', &
'  GAMMA(3) - [MATHEMATICS] Gamma function, which yields factorials for', &
'  positive whole numbers', &
'', &
'SYNOPSIS', &
'  result = gamma(x)', &
'', &
'           elemental real(kind=KIND) function gamma( x)', &
'', &
'            type(real,kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is a real value', &
'', &
'  o  returns a real value with the kind as X.', &
'', &
'DESCRIPTION', &
'  GAMMA(X) computes Gamma of X. For positive whole number values of N the', &
'  Gamma function can be used to calculate factorials, as (N-1)! ==', &
'  GAMMA(REAL(N)). That is', &
'', &
'      n! == gamma(real(n+1))', &
'', &
'  $$ \GAMMA(x) = \int_0**\infty t**{x-1}{\mathrm{e}}**{-T}\,{\mathrm{d}}t $$', &
'', &
'OPTIONS', &
'  o  X : Shall be of type real and neither zero nor a negative integer.', &
'', &
'RESULT', &
'  The return value is of type real of the same kind as x. The result has a', &
'  value equal to a processor-dependent approximation to the gamma function of', &
'  X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_gamma', &
'      use, intrinsic :: iso_fortran_env, only : wp=>real64', &
'      implicit none', &
'      real :: x, xa(4)', &
'      integer :: i', &
'', &
'         x = gamma(1.0)', &
'         write(*,*)''gamma(1.0)='',x', &
'', &
'         ! elemental', &
'         xa=gamma([1.0,2.0,3.0,4.0])', &
'         write(*,*)xa', &
'         write(*,*)', &
'', &
'         ! gamma(3) is related to the factorial function', &
'         do i=1,20', &
'            ! check value is not too big for default integer type', &
'            if(factorial(i).gt.huge(0))then', &
'               write(*,*)i,factorial(i)', &
'            else', &
'               write(*,*)i,factorial(i),int(factorial(i))', &
'            endif', &
'         enddo', &
'         ! more factorials', &
'         FAC: block', &
'         integer,parameter :: n(*)=[0,1,5,11,170]', &
'         integer :: j', &
'            do j=1,size(n)', &
'               write(*,''(*(g0,1x))'')''factorial of'', n(j),'' is '', &', &
'                & product([(real(i,kind=wp),i=1,n(j))]),  &', &
'                & gamma(real(n(j)+1,kind=wp))', &
'            enddo', &
'         endblock FAC', &
'', &
'         contains', &
'         function factorial(i) result(f)', &
'         integer,parameter :: dp=kind(0d0)', &
'         integer,intent(in) :: i', &
'         real :: f', &
'            if(i.le.0)then', &
'               write(*,''(*(g0))'')''<ERROR> gamma(3) function value '',i,'' <= 0''', &
'               stop      ''<STOP> bad value in gamma function''', &
'            endif', &
'            f=gamma(real(i+1))', &
'         end function factorial', &
'      end program demo_gamma', &
'', &
'  Results:', &
'', &
'          gamma(1.0)=   1.000000', &
'            1.000000       1.000000       2.000000       6.000000', &
'', &
'                    1   1.000000               1', &
'                    2   2.000000               2', &
'                    3   6.000000               6', &
'                    4   24.00000              24', &
'                    5   120.0000             120', &
'                    6   720.0000             720', &
'                    7   5040.000            5040', &
'                    8   40320.00           40320', &
'                    9   362880.0          362880', &
'                   10   3628800.         3628800', &
'                   11  3.9916800E+07    39916800', &
'                   12  4.7900160E+08   479001600', &
'                   13  6.2270208E+09', &
'                   14  8.7178289E+10', &
'                   15  1.3076744E+12', &
'                   16  2.0922791E+13', &
'                   17  3.5568741E+14', &
'                   18  6.4023735E+15', &
'                   19  1.2164510E+17', &
'                   20  2.4329020E+18', &
'', &
'    factorial of 0', &
'      is  1.000000000000000 1.000000000000000', &
'', &
'    factorial of 1', &
'      is  1.000000000000000 1.000000000000000', &
'', &
'    factorial of 5', &
'      is  120.0000000000000 120.0000000000000', &
'', &
'    factorial of 11', &
'      is  39916800.00000000 39916800.00000000', &
'', &
'    factorial of 170', &
'      is  .7257415615307994E+307 .7257415615307999E+307', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  Logarithm of the Gamma function: LOG_GAMMA(3)', &
'', &
'RESOURCES', &
'  Wikipedia: Gamma_function', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022               gamma(3fortran)', &
'']

shortname="gamma"
call process()


case('89','get_command')

textblock=[character(len=256) :: &
'', &
'get_command(3fortran)                                    get_command(3fortran)', &
'', &
'NAME', &
'  GET_COMMAND(3) - [SYSTEM:COMMAND LINE] Get the entire command line', &
'  invocation', &
'', &
'SYNOPSIS', &
'  call get_command([command] [,length] [,status] [,errmsg])', &
'', &
'           subroutine get_command( command ,length ,status, errmsg )', &
'', &
'            character(len=*),intent(out),optional   :: command', &
'            integer(kind=**),intent(out),optional   :: length', &
'            integer(kind=**),intent(out),optional   :: status', &
'            character(len=*),intent(inout),optional :: errmsg', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type meeting', &
'     the conditions described herein.', &
'', &
'  o  COMMAND and ERRMSG are scalar character variables of default kind.', &
'', &
'  o  LENGTH and STATUS are scalar integer with a decimal exponent range of at', &
'     least four.', &
'', &
'DESCRIPTION', &
'  GET_COMMAND(3) retrieves the entire command line that was used to invoke the', &
'  program.', &
'', &
'  Note that what is typed on the command line is often processed by a shell.', &
'  The shell typically processes special characters and white space before', &
'  passing it to the program. The processing can typically be turned off by', &
'  turning off globbing or quoting the command line arguments and/or changing', &
'  the default field separators, but this should rarely be necessary.', &
'', &
'RESULT', &
'  o  COMMAND : If COMMAND is present, the entire command line that was used to', &
'     invoke the program is stored into it. If the command cannot be', &
'     determined, COMMAND is assigned all blanks.', &
'', &
'  o  LENGTH : If LENGTH is present, it is assigned the length of the command', &
'     line. It is system-dependent as to whether trailing blanks will be', &
'     counted. : If the command length cannot be determined, a length of 0 is', &
'     assigned.', &
'', &
'  o  STATUS : If STATUS is present, it is assigned 0 upon success of the', &
'     command, -1 if COMMAND is too short to store the command line, or a', &
'     positive value in case of an error.', &
'', &
'  o  ERRMSG : It is assigned a processor-dependent explanatory message if the', &
'     command retrieval fails. Otherwise, it is unchanged.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_get_command', &
'      implicit none', &
'      integer                      :: command_line_length', &
'      character(len=:),allocatable :: command_line', &
'         ! get command line length', &
'         call get_command(length=command_line_length)', &
'         ! allocate string big enough to hold command line', &
'         allocate(character(len=command_line_length) :: command_line)', &
'         ! get command line as a string', &
'         call get_command(command=command_line)', &
'         ! trim leading spaces just in case', &
'         command_line=adjustl(command_line)', &
'         write(*,''("OUTPUT:",a)'')command_line', &
'      end program demo_get_command', &
'', &
'  Results:', &
'', &
'           # note that shell expansion removes some of the whitespace', &
'           # without quotes', &
'           ./test_get_command  arguments    on command   line to   echo', &
'', &
'           OUTPUT:./test_get_command arguments on command line to echo', &
'', &
'           # using the bash shell with single quotes', &
'           ./test_get_command  ''arguments  *><`~[]!{}?"\''| ''', &
'', &
'           OUTPUT:./test_get_command arguments  *><`~[]!{}?"''|', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  GET_COMMAND_ARGUMENT(3), COMMAND_ARGUMENT_COUNT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022         get_command(3fortran)', &
'']

shortname="get_command"
call process()


case('90','get_command_argument')

textblock=[character(len=256) :: &
'', &
'get_command_argument(3fortran)                  get_command_argument(3fortran)', &
'', &
'NAME', &
'  GET_COMMAND_ARGUMENT(3) - [SYSTEM:COMMAND LINE] Get command line arguments', &
'', &
'SYNOPSIS', &
'  call get_command_argument(number [,value] [,length] & & [,status] [,errmsg])', &
'', &
'         subroutine get_command_argument( number, value, length, &', &
'         & status ,errmsg)', &
'', &
'          integer(kind=**),intent(in)             :: number', &
'          character(len=*),intent(out),optional   :: value', &
'          integer(kind=**),intent(out),optional   :: length', &
'          integer(kind=**),intent(out),optional   :: status', &
'          character(len=*),intent(inout),optional :: errmsg', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type meeting', &
'     the conditions described herein.', &
'', &
'  o  NUMBER, LENGTH, and STATUS are scalar integer with a decimal exponent', &
'     range of at least four.', &
'', &
'  o  VALUE and ERRMSG are scalar character variables of default kind.', &
'', &
'DESCRIPTION', &
'  GET_COMMAND_ARGUMENT(3) retrieves or queries the n-th argument that was', &
'  passed on the command line to the current program execution.', &
'', &
'  There is not anything specifically stated about what an argument is but in', &
'  practice the arguments are strings split on whitespace unless the arguments', &
'  are quoted. IFS values (Internal Field Separators) used by common shells are', &
'  typically ignored and unquoted whitespace is almost always the separator.', &
'', &
'  Shells have often expanded command arguments and spell characters before', &
'  passing them to the program, so the strings read are often not exactly what', &
'  the user typed on the command line.', &
'', &
'OPTIONS', &
'  o  NUMBER : is a non-negative number indicating which argument of the', &
'     current program command line is to be retrieved or queried. : If NUMBER =', &
'     0, the argument pointed to is set to the name of the program (on systems', &
'     that support this feature). : if the processor does not have such a', &
'     concept as a command name the value of command argument 0 is processor', &
'     dependent. : For values from 1 to the number of arguments passed to the', &
'     program a value is returned in an order determined by the processor.', &
'     Conventionally they are returned consecutively as they appear on the', &
'     command line from left to right.', &
'', &
'RESULT', &
'  o  VALUE : The VALUE argument holds the command line argument. If VALUE can', &
'     not hold the argument, it is truncated to fit the length of VALUE. : If', &
'     there are less than NUMBER arguments specified at the command line or if', &
'     the argument specified does not exist for other reasons, VALUE will be', &
'     filled with blanks.', &
'', &
'  o  LENGTH : The LENGTH argument contains the length of the n-th command line', &
'     argument. The length of VALUE has no effect on this value, It is the', &
'     length required to hold all the significant characters of the argument', &
'     regardless of how much storage is provided by VALUE.', &
'', &
'  o  STATUS : If the argument retrieval fails, STATUS is a positive number; if', &
'     VALUE contains a truncated command line argument, STATUS is -1; and', &
'     otherwise the STATUS is zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_get_command_argument', &
'      implicit none', &
'      character(len=255)           :: progname', &
'      integer                      :: count, i, argument_length, istat', &
'      character(len=:),allocatable :: arg', &
'', &
'       ! command name assuming it is less than 255 characters in length', &
'        call get_command_argument (0, progname, status=istat)', &
'        if (istat == 0) then', &
'           print *, "The program''s name is " // trim (progname)', &
'        else', &
'           print *, "Could not get the program''s name " // trim (progname)', &
'        endif', &
'', &
'       ! get number of arguments', &
'        count = command_argument_count()', &
'        write(*,*)''The number of arguments is '',count', &
'', &
'        !', &
'        ! allocate string array big enough to hold command line', &
'        ! argument strings and related information', &
'        !', &
'        do i=1,count', &
'           call get_command_argument(number=i,length=argument_length)', &
'           if(allocated(arg))deallocate(arg)', &
'           allocate(character(len=argument_length) :: arg)', &
'           call get_command_argument(i, arg,status=istat)', &
'           ! show the results', &
'           write (*,''(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")'') &', &
'           & i,istat,argument_length,arg', &
'        enddo', &
'', &
'      end program demo_get_command_argument', &
'', &
'  Results:', &
'', &
'       ./demo_get_command_argument a  test ''of getting  arguments '' " leading"', &
'', &
'       The program''s name is ./demo_get_command_argument', &
'       The number of arguments is            4', &
'  001 00000 00001 [a] 002 00000 00004 [test]', &
'', &
'    003 00000 00022 [of getting', &
'      arguments ] 004 00000 00008 [ leading]', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  GET_COMMAND(3), COMMAND_ARGUMENT_COUNT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022get_command_argument(3fortran)', &
'']

shortname="get_command_argument"
call process()


case('91','get_environment_variable')

textblock=[character(len=256) :: &
'', &
'get_environment_variable(3fortran)          get_environment_variable(3fortran)', &
'', &
'NAME', &
'  GET_ENVIRONMENT_VARIABLE(3) - [SYSTEM:ENVIRONMENT] Get value of an', &
'  environment variable', &
'', &
'SYNOPSIS', &
'  call get_environment_variable(name [,value] [,length] & & [,status]', &
'  [,trim_name] [,errmsg] )', &
'', &
'           subroutine character(len=*) get_environment_variable( &', &
'           & name, value, length, status, trim_name, errmsg )', &
'', &
'            character(len=*),intent(in) :: name', &
'            character(len=*),intent(out),optional   :: value', &
'            integer(kind=**),intent(out),optional   :: length', &
'            integer(kind=**),intent(out),optional   :: status', &
'            logical,intent(out),optional            :: trim_name', &
'            character(len=*),intent(inout),optional :: errmsg', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type meeting', &
'     the conditions described herein.', &
'', &
'  o  NAME, VALUE, and ERRMSG are a scalar character of default kind.', &
'', &
'  o  LENGTH and STATUS are integer scalars with a decimal exponent range of at', &
'     least four.', &
'', &
'  o  TRIM_NAME is a scalar of type logical and of default kind.', &
'', &
'DESCRIPTION', &
'  GET_ENVIRONMENT_VARIABLE(3) gets the VALUE of the environment variable NAME.', &
'', &
'  Note that GET_ENVIRONMENT_VARIABLE(3) need not be thread-safe. It is the', &
'  responsibility of the user to ensure that the environment is not being', &
'  updated concurrently.', &
'', &
'  If running in parallel be aware It is processor dependent whether an', &
'  environment variable that exists on an image also exists on another image,', &
'  and if it does exist on both images whether the values are the same or', &
'  different.', &
'', &
'OPTIONS', &
'  o  NAME : The name of the environment variable to query. The interpretation', &
'     of case is processor dependent.', &
'', &
'RESULT', &
'  o  VALUE : The value of the environment variable being queried. If VALUE is', &
'     not large enough to hold the data, it is truncated. If the variable NAME', &
'     is not set or has no value, or the processor does not support environment', &
'     variables VALUE will be filled with blanks.', &
'', &
'  o  LENGTH : Argument LENGTH contains the length needed for storing the', &
'     environment variable NAME. It is zero if the environment variable is not', &
'     set.', &
'', &
'  o  STATUS : STATUS is -1 if VALUE is present but too short for the', &
'     environment variable; it is 1 if the environment variable does not exist', &
'     and 2 if the processor does not support environment variables; in all', &
'     other cases STATUS is zero.', &
'', &
'  o  TRIM_NAME : If TRIM_NAME is present with the value .false., the trailing', &
'     blanks in NAME are significant; otherwise they are not part of the', &
'     environment variable name.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_getenv', &
'      implicit none', &
'      character(len=:),allocatable :: homedir', &
'      character(len=:),allocatable :: var', &
'', &
'           var=''HOME''', &
'           homedir=get_env(var)', &
'           write (*,''(a,"=""",a,"""")'')var,homedir', &
'', &
'      contains', &
'', &
'      function get_env(name,default) result(value)', &
'      ! a function that makes calling get_environment_variable(3) simple', &
'      implicit none', &
'      character(len=*),intent(in)          :: name', &
'      character(len=*),intent(in),optional :: default', &
'      character(len=:),allocatable         :: value', &
'      integer                              :: howbig', &
'      integer                              :: stat', &
'      integer                              :: length', &
'         length=0', &
'         value=''''', &
'         if(name.ne.'''')then', &
'            call get_environment_variable( name, &', &
'            & length=howbig,status=stat,trim_name=.true.)', &
'            select case (stat)', &
'            case (1)', &
'             print *, name, " is not defined in the environment. Strange..."', &
'             value=''''', &
'            case (2)', &
'             print *, &', &
'             "This processor does not support environment variables. Boooh!"', &
'             value=''''', &
'            case default', &
'             ! make string of sufficient size to hold value', &
'             if(allocated(value))deallocate(value)', &
'             allocate(character(len=max(howbig,1)) :: value)', &
'             ! get value', &
'             call get_environment_variable( &', &
'             & name,value,status=stat,trim_name=.true.)', &
'             if(stat.ne.0)value=''''', &
'            end select', &
'         endif', &
'         if(value.eq.''''.and.present(default))value=default', &
'      end function get_env', &
'', &
'      end program demo_getenv', &
'', &
'  Typical Results:', &
'', &
'         HOME="/home/urbanjs"', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  GET_COMMAND_ARGUMENT(3), GET_COMMAND(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, get_environment_variable(3fortran)', &
'']

shortname="get_environment_variable"
call process()


case('92','huge')

textblock=[character(len=256) :: &
'', &
'huge(3fortran)                                                  huge(3fortran)', &
'', &
'NAME', &
'  HUGE(3) - [NUMERIC MODEL] Largest number of a type and kind', &
'', &
'SYNOPSIS', &
'  result = huge(x)', &
'', &
'           TYPE(kind=KIND) function huge(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x(..)', &
'', &
'CHARACTERISTICS', &
'  o  X may be any real or integer scalar or array and any kind.', &
'', &
'  o  The result will be a scalar of the same type and kind as the input X', &
'', &
'DESCRIPTION', &
'  HUGE(3) returns the largest number that is not an overflow for the kind and', &
'  type of X.', &
'', &
'OPTIONS', &
'  o  X : X is an arbitrary value which is used merely to determine what kind', &
'     and type of scalar is being queried. It need not be defined, as only its', &
'     characteristics are used.', &
'', &
'RESULT', &
'  The result is the largest value supported by the specified type and kind.', &
'', &
'  Note the result is as the same kind as the input to ensure the returned', &
'  value does not overflow. Any assignment of the result to a variable should', &
'  take this into consideration.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_huge', &
'      implicit none', &
'      character(len=*),parameter :: f=''(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)''', &
'      integer :: i,j,k,biggest', &
'      real :: v, w', &
'         ! basic', &
'         print *, huge(0), huge(0.0), huge(0.0d0)', &
'         print *, tiny(0.0), tiny(0.0d0)', &
'', &
'         ! advanced', &
'         biggest=huge(0)', &
'         ! be careful of overflow when using integers in computation', &
'         do i=1,14', &
'            j=6**i   ! Danger, Danger', &
'            w=6**i   ! Danger, Danger', &
'            v=6.0**i', &
'            k=v      ! Danger, Danger', &
'            if(v.gt.biggest)then', &
'               write(*,f) i, j, k, v, v.eq.w, ''wrong j and k and w''', &
'            else', &
'               write(*,f) i, j, k, v, v.eq.w', &
'            endif', &
'         enddo', &
'      end program demo_huge', &
'', &
'  Results:', &
'', &
'        2147483647  3.4028235E+38  1.797693134862316E+308', &
'        1.1754944E-38  2.225073858507201E-308', &
'', &
'          1      6           6             6. T', &
'          2      36          36            36. T', &
'          3      216         216           216. T', &
'          4      1296        1296          1296. T', &
'          5      7776        7776          7776. T', &
'          6      46656       46656         46656. T', &
'          7      279936      279936        279936. T', &
'          8      1679616     1679616       1679616. T', &
'          9      10077696    10077696      10077696. T', &
'          10     60466176    60466176      60466176. T', &
'          11     362797056   362797056     362797056. T', &
'          12    -2118184960 -2147483648    2176782336. F wrong for j and k and w', &
'          13     175792128  -2147483648   13060694016. F wrong for j and k and w', &
'          14     1054752768 -2147483648   78364164096. F wrong for j and k and w', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SCALE(3), SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                huge(3fortran)', &
'']

shortname="huge"
call process()


case('93','hypot')

textblock=[character(len=256) :: &
'', &
'hypot(3fortran)                                                hypot(3fortran)', &
'', &
'NAME', &
'  HYPOT(3) - [MATHEMATICS] Returns the Euclidean distance - the distance', &
'  between a point and the origin.', &
'', &
'SYNOPSIS', &
'  result = hypot(x, y)', &
'', &
'           elemental real(kind=KIND) function hypot(x,y)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'            real(kind=KIND),intent(in) :: y', &
'', &
'CHARACTERISTICS', &
'  o  X,Y and the result shall all be real and of the same KIND.', &
'', &
'DESCRIPTION', &
'  HYPOT(3) is referred to as the Euclidean distance function. It is equal to', &
'', &
'      sqrt(x**2+y**2)', &
'', &
'  without undue underflow or overflow.', &
'', &
'  In mathematics, the Euclidean distance between two points in Euclidean space', &
'  is the length of a line segment between two points.', &
'', &
'  HYPOT(X,Y) returns the distance between the point <X,Y> and the origin.', &
'', &
'OPTIONS', &
'  o  X : The type shall be real.', &
'', &
'  o  Y : The type and kind type parameter shall be the same as X.', &
'', &
'RESULT', &
'  The return value has the same type and kind type parameter as X.', &
'', &
'  The result is the positive magnitude of the distance of the point <X,Y> from', &
'  the origin <0.0,0.0> .', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_hypot', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'       & real_kinds, real32, real64, real128', &
'      implicit none', &
'      real(kind=real32) :: x, y', &
'      real(kind=real32),allocatable :: xs(:), ys(:)', &
'      integer :: i', &
'      character(len=*),parameter :: f=''(a,/,SP,*(3x,g0,1x,g0:,/))''', &
'', &
'         x = 1.e0_real32', &
'         y = 0.5e0_real32', &
'', &
'         write(*,*)', &
'         write(*,''(*(g0))'')''point <'',x,'','',y,''> is '',hypot(x,y)', &
'         write(*,''(*(g0))'')''units away from the origin''', &
'         write(*,*)', &
'', &
'         ! elemental', &
'         xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]', &
'         ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]', &
'', &
'         write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))', &
'         write(*,f)"have distances from the origin of ",hypot(xs,ys)', &
'         write(*,f)"the closest is",minval(hypot(xs,ys))', &
'', &
'      end program demo_hypot', &
'', &
'  Results:', &
'', &
'         point <1.00000000,0.500000000> is 1.11803401', &
'         units away from the origin', &
'', &
'         the points', &
'            +1.00000000 +0.500000000', &
'            +1.00000000 +0.250000000', &
'            +10.0000000 -10.0000000', &
'            +15.0000000 +0.250000000', &
'            -1.00000000 -0.250000000', &
'         have distances from the origin of', &
'            +1.11803401 +1.03077638', &
'            +14.1421356 +15.0020828', &
'            +1.03077638', &
'         the closest is', &
'            +1.03077638', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               hypot(3fortran)', &
'']

shortname="hypot"
call process()


case('94','iachar')

textblock=[character(len=256) :: &
'', &
'iachar(3fortran)                                              iachar(3fortran)', &
'', &
'NAME', &
'  IACHAR(3) - [CHARACTER:CONVERSION] Return integer ASCII code of a character', &
'', &
'SYNOPSIS', &
'  result = iachar(c [,kind])', &
'', &
'           elemental integer(kind=KIND) function iachar(c,kind)', &
'', &
'            character(len=1),intent(in) :: c', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  C is a single character', &
'', &
'  o  The return value is of type integer and of kind KIND. If KIND is absent,', &
'     the return value is of default integer kind.', &
'', &
'  NOTE: : a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  IACHAR(3) returns the code for the ASCII character in the first character', &
'  position of C.', &
'', &
'OPTIONS', &
'  o  C : A character to determine the ASCII code of. : A common extension is', &
'     to allow strings but all but the first character is then ignored.', &
'', &
'  o  KIND : A constant initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'RESULT', &
'  the result is the position of the character C in the ASCII collating', &
'  sequence. It is nonnegative and less than or equal to 127.', &
'', &
'  By ASCII, it is meant that C is in the collating sequence defined by the', &
'  codes specified in ISO/IEC 646:1991 (International Reference Version).', &
'', &
'  The value of the result is processor dependent if C is not in the ASCII', &
'  collating sequence.', &
'', &
'  The results are consistent with the LGE(3), LGT(3), LLE(3), and LLT(3)', &
'  comparison functions. For example, if LLE(C, D) is true, IACHAR(C) <= IACHAR', &
'  (D) is true where C and D are any two characters representable by the', &
'  processor.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_iachar', &
'      implicit none', &
'         ! basic usage', &
'          ! just does a string one character long', &
'          write(*,*)iachar(''A'')', &
'          ! elemental: can do an array of letters', &
'          write(*,*)iachar([''A'',''Z'',''a'',''z''])', &
'', &
'         ! convert all characters to lowercase', &
'          write(*,''(a)'')lower(''abcdefg ABCDEFG'')', &
'      contains', &
'      !', &
'      pure elemental function lower(str) result (string)', &
'      ! Changes a string to lowercase', &
'      character(*), intent(In)     :: str', &
'      character(len(str))          :: string', &
'      integer                      :: i', &
'         string = str', &
'         ! step thru each letter in the string in specified range', &
'         do i = 1, len(str)', &
'            select case (str(i:i))', &
'            case (''A'':''Z'') ! change letter to miniscule', &
'               string(i:i) = char(iachar(str(i:i))+32)', &
'            case default', &
'            end select', &
'         end do', &
'      end function lower', &
'      !', &
'      end program demo_iachar', &
'', &
'  Results:', &
'', &
'         65', &
'         65          90          97         122', &
'         abcdefg abcdefg', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  ACHAR(3), CHAR(3), ICHAR(3)', &
'', &
'  See ICHAR(3) in particular for a discussion of converting between numerical', &
'  values and formatted string representations.', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              iachar(3fortran)', &
'']

shortname="iachar"
call process()


case('95','iall')

textblock=[character(len=256) :: &
'', &
'iall(3fortran)                                                  iall(3fortran)', &
'', &
'NAME', &
'  IALL(3) - [BIT:LOGICAL] Bitwise and of array elements', &
'', &
'SYNOPSIS', &
'  result = iall(array [,mask]) | iall(array ,dim [,mask])', &
'', &
'           integer(kind=KIND) function iall(array,dim,mask)', &
'', &
'            integer(kind=KIND),intent(in)        :: array(*)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(*)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  ARRAY must be an integer array', &
'', &
'  o  MASK is a logical array that conforms to ARRAY of any logical kind.', &
'', &
'  o  DIM may be of any integer kind.', &
'', &
'  o  The result will by of the same type and kind as ARRAY.', &
'', &
'DESCRIPTION', &
'  IALL(3) reduces with a bitwise and the elements of ARRAY along dimension DIM', &
'  if the corresponding element in MASK is .true..', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array of type integer', &
'', &
'  o  DIM : (Optional) shall be a scalar of type integer with a value in the', &
'     range from 1 TO N, where N equals the rank of ARRAY.', &
'', &
'  o  MASK : (Optional) shall be of type logical and either be a scalar or an', &
'     array of the same shape as ARRAY.', &
'', &
'RESULT', &
'  The result is of the same type as ARRAY.', &
'', &
'  If DIM is absent, a scalar with the bitwise all of all elements in ARRAY is', &
'  returned. Otherwise, an array of rank N-1, where N equals the rank of ARRAY,', &
'  and a shape similar to that of ARRAY with dimension DIM dropped is returned.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_iall', &
'      use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'       & int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int8) :: a(2)', &
'', &
'         a(1) = int(b''00100100'')', &
'         a(2) = int(b''01101010'')', &
'', &
'         print ''(b8.8)'', iall(a)', &
'', &
'      end program demo_iall', &
'', &
'  Results:', &
'', &
'       > 00100000', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  IANY(3), IPARITY(3), IAND(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                iall(3fortran)', &
'']

shortname="iall"
call process()


case('96','iand')

textblock=[character(len=256) :: &
'', &
'iand(3fortran)                                                  iand(3fortran)', &
'', &
'NAME', &
'  IAND(3) - [BIT:LOGICAL] Bitwise logical AND', &
'', &
'SYNOPSIS', &
'  result = iand(i, j)', &
'', &
'           elemental integer(kind=KIND) function iand(i,j)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=KIND),intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  I, J and the result shall have the same integer type and kind, with the', &
'     exception that one of I or J may be a BOZ constant.', &
'', &
'DESCRIPTION', &
'  IAND(3) returns the bitwise logical AND of two values.', &
'', &
'OPTIONS', &
'  o  I : one of the pair of values to compare the bits of', &
'', &
'  o  J : one of the pair of values to compare the bits of', &
'', &
'  If either I or J is a BOZ-literal-constant, it is first converted as if by', &
'  the intrinsic function INT(3) to type integer with the kind type parameter', &
'  of the other.', &
'', &
'RESULT', &
'  The result has the value obtained by combining I and I bit-by-bit according', &
'  to the following table:', &
'', &
'          I  |  J  |  IAND (I, J)', &
'  ----------------------------', &
'', &
'    1 |  1  |    1', &
'', &
'    1 |  0  |    0', &
'', &
'    0 |  1  |    0', &
'', &
'    0 |  0  |    0', &
'', &
'  So if both the bit in I and J are on the resulting bit is on (a one); else', &
'  the resulting bit is off (a zero).', &
'', &
'  This is commonly called the "bitwise logical AND" of the two values.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_iand', &
'      implicit none', &
'      integer :: a, b', &
'       data a / z''f'' /, b / z''3'' /', &
'       write (*,*) ''a='',a,'' b='',b,''iand(a,b)='',iand(a, b)', &
'       write (*,''(b32.32)'') a,b,iand(a,b)', &
'      end program demo_iand', &
'', &
'  Results:', &
'', &
'          a= 15  b= 3 iand(a,b)= 3', &
'  00000000000000000000000000001111 00000000000000000000000000000011', &
'  00000000000000000000000000000011', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3), IOR(3),', &
'  IEOR(3), MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                iand(3fortran)', &
'']

shortname="iand"
call process()


case('97','iany')

textblock=[character(len=256) :: &
'', &
'iany(3fortran)                                                  iany(3fortran)', &
'', &
'NAME', &
'  IANY(3) - [BIT:LOGICAL] Bitwise OR of array elements', &
'', &
'SYNOPSIS', &
'  result = iany(array [,mask]) | iany(array ,dim [,mask])', &
'', &
'           integer(kind=KIND) function iany(array,dim,mask)', &
'', &
'            integer(kind=KIND),intent(in)        :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY is an integer array', &
'', &
'  o  DIM may be of any integer kind.', &
'', &
'  o  MASK is a logical array that conforms to ARRAY', &
'', &
'  o  The result will by of the same type and kind as ARRAY. It is scalar if', &
'     DIM does not appear or is 1. Otherwise, it is the shape and rank of array', &
'     reduced by the dimension DIM.', &
'', &
'  note a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  IANY(3) reduces with bitwise OR (inclusive OR) the elements of ARRAY along', &
'  dimension DIM if the corresponding element in MASK is .true..', &
'', &
'OPTIONS', &
'  o  ARRAY : an array of elements to selectively OR based on the mask.', &
'', &
'  o  DIM : a value in the range from 1 TO N, where N equals the rank of ARRAY.', &
'', &
'  o  MASK : a logical scalar; or an array of the same shape as ARRAY.', &
'', &
'RESULT', &
'  The result is of the same type as ARRAY.', &
'', &
'  If DIM is absent, a scalar with the bitwise or of all elements in ARRAY is', &
'  returned. Otherwise, an array of rank N-1, where N equals the rank of ARRAY,', &
'  and a shape similar to that of ARRAY with dimension DIM dropped is returned.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_iany', &
'      use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'       & int8, int16, int32, int64', &
'      implicit none', &
'      logical,parameter :: T=.true., F=.false.', &
'      integer(kind=int8) :: a(3)', &
'         a(1) = int(b''00100100'',int8)', &
'         a(2) = int(b''01101010'',int8)', &
'         a(3) = int(b''10101010'',int8)', &
'         write(*,*)''A=''', &
'         print ''(1x,b8.8)'', a', &
'         print *', &
'         write(*,*)''IANY(A)=''', &
'         print ''(1x,b8.8)'', iany(a)', &
'         print *', &
'         write(*,*)''IANY(A) with a mask''', &
'         print ''(1x,b8.8)'', iany(a,mask=[T,F,T])', &
'         print *', &
'         write(*,*)''should match ''', &
'         print ''(1x,b8.8)'', iany([a(1),a(3)])', &
'         print *', &
'         write(*,*)''does it?''', &
'         write(*,*)iany(a,[T,F,T]) == iany([a(1),a(3)])', &
'      end program demo_iany', &
'', &
'  Results:', &
'', &
'          A=', &
'          00100100', &
'          01101010', &
'          10101010', &
'', &
'          IANY(A)=', &
'          11101110', &
'', &
'          IANY(A) with a mask', &
'          10101110', &
'', &
'          should match', &
'          10101110', &
'', &
'          does it?', &
'', &
'   T', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  IPARITY(3), IALL(3), IOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                iany(3fortran)', &
'']

shortname="iany"
call process()


case('98','ibclr')

textblock=[character(len=256) :: &
'', &
'ibclr(3fortran)                                                ibclr(3fortran)', &
'', &
'NAME', &
'  IBCLR(3) - [BIT:SET] Clear a bit', &
'', &
'SYNOPSIS', &
'  result = ibclr(i, pos)', &
'', &
'           elemental integer(kind=KIND) function ibclr(i,pos)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: pos', &
'', &
'CHARACTERISTICS', &
'  o  I shall be type integer.', &
'', &
'  o  POS shall be type integer.', &
'', &
'  o  The return value is of the same kind as I.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  IBCLR(3) returns the value of I with the bit at position POS set to zero.', &
'', &
'OPTIONS', &
'  o  I : The initial value to be modified', &
'', &
'  o  POS : The position of the bit to change in the input value. A value of', &
'     zero refers to the right-most bit. The value of POS must be nonnegative', &
'     and less than (BIT_SIZE(I)).', &
'', &
'RESULT', &
'  The returned value has the same bit sequence as I except the designated bit', &
'  is unconditionally set to 0', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ibclr', &
'      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int16) :: i', &
'        ! basic usage', &
'         print *,ibclr (16, 1), '' ==> ibclr(16,1) has the value 15''', &
'', &
'         ! it is easier to see using binary representation', &
'         i=int(b''0000000000111111'',kind=int16)', &
'         write(*,''(b16.16,1x,i0)'') ibclr(i,3), ibclr(i,3)', &
'', &
'        ! elemental', &
'         print *,''an array of initial values may be given as well''', &
'         print *,ibclr(i=[7,4096,9], pos=2)', &
'         print *', &
'         print *,''a list of positions results in multiple returned values''', &
'         print *,''not multiple bits set in one value, as the routine is  ''', &
'         print *,''a scalar function; calling it elementally essentially  ''', &
'         print *,''calls it multiple times.                               ''', &
'         write(*,''(b16.16)'') ibclr(i=-1_int16, pos=[1,2,3,4])', &
'', &
'         ! both may be arrays if of the same size', &
'', &
'      end program demo_ibclr', &
'', &
'  Results:', &
'', &
'       >           16  ==> ibclr(16,1) has the value 15', &
'       > 0000000000110111 55', &
'       >  an array of initial values may be given as well', &
'       >            3        4096           9', &
'       >', &
'       >  a list of positions results in multiple returned values', &
'       >  not multiple bits set in one value, as the routine is', &
'       >  a scalar function; calling it elementally essentially', &
'       >  calls it multiple times.', &
'       > 1111111111111101', &
'       > 1111111111111011', &
'       > 1111111111110111', &
'       > 1111111111101111', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), NOT(3), BTEST(3), IBSET(3), IBITS(3), IAND(3), IOR(3), IEOR(3),', &
'  MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               ibclr(3fortran)', &
'']

shortname="ibclr"
call process()


case('99','ibits')

textblock=[character(len=256) :: &
'', &
'ibits(3fortran)                                                ibits(3fortran)', &
'', &
'NAME', &
'  IBITS(3) - [BIT:COPY] Extraction of a subset of bits', &
'', &
'SYNOPSIS', &
'  result = ibits(i, pos, len)', &
'', &
'           elemental integer(kind=KIND) function ibits(i,pos,len)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: pos', &
'            integer(kind=**),intent(in) :: len', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported integer kind', &
'', &
'  o  I may be any supported integer kind as well', &
'', &
'  o  the return value will be the same kind as I', &
'', &
'DESCRIPTION', &
'  IBITS(3) extracts a field of bits from I, starting from bit position POS and', &
'  extending left for a total of LEN bits.', &
'', &
'  The result is then right-justified and the remaining left-most bits in the', &
'  result are zeroed.', &
'', &
'  The position POS is calculated assuming the right-most bit is zero and the', &
'  positions increment to the left.', &
'', &
'OPTIONS', &
'  o  I : The value to extract bits from', &
'', &
'  o  POS : The position of the bit to start copying at. POS is non-negative.', &
'', &
'  o  LEN : the number of bits to copy from I. It must be non-negative.', &
'', &
'  POS + LEN shall be less than or equal to BIT_SIZE(I).', &
'', &
'RESULT', &
'  The return value is composed of the selected bits right-justified, left-', &
'  padded with zeros.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ibits', &
'      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int16) :: i,j', &
'        ! basic usage', &
'         print *,ibits (14, 1, 3) ! should be seven', &
'         print *,ibits(-1,10,3)   ! and so is this', &
'         ! it is easier to see using binary representation', &
'         i=int(b''0101010101011101'',kind=int16)', &
'         write(*,''(b16.16,1x,i0)'') ibits(i,3,3), ibits(i,3,3)', &
'', &
'        ! we can illustrate this as', &
'         !        #-- position 15', &
'         !        |              #-- position 0', &
'         !        |   <-- +len   |', &
'         !        V              V', &
'         !        5432109876543210', &
'         i =int(b''1111111111111111'',kind=int16)', &
'         !          ^^^^', &
'         j=ibits(i,10,4) ! start at 10th from left and proceed', &
'                         ! left for a total of 4 characters', &
'         write(*,''(a,b16.16)'')''j='',j', &
'        ! lets do something less ambiguous', &
'         i =int(b''0010011000000000'',kind=int16)', &
'         j=ibits(i,9,5)', &
'         write(*,''(a,b16.16)'')''j='',j', &
'      end program demo_ibits', &
'', &
'  Results:', &
'', &
'       > 7', &
'       > 7', &
'       > 0000000000000011 3', &
'       > j=0000000000001111', &
'       > j=0000000000010011', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBSET(3), IAND(3), IOR(3),', &
'  IEOR(3), MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               ibits(3fortran)', &
'']

shortname="ibits"
call process()


case('100','ibset')

textblock=[character(len=256) :: &
'', &
'ibset(3fortran)                                                ibset(3fortran)', &
'', &
'NAME', &
'  IBSET(3) - [BIT:SET] Set a bit to one in an integer value', &
'', &
'SYNOPSIS', &
'  result = ibset(i, pos)', &
'', &
'           elemental integer(kind=KIND) function ibset(i,pos)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: pos', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  The return value is of the same kind as I. Otherwise, any integer kinds', &
'     are allowed.', &
'', &
'DESCRIPTION', &
'  IBSET(3) returns the value of I with the bit at position POS set to one.', &
'', &
'OPTIONS', &
'  o  I : The initial value to be modified', &
'', &
'  o  POS : The position of the bit to change in the input value. A value of', &
'     zero refers to the right-most bit. The value of POS must be nonnegative', &
'     and less than (BIT_SIZE(I)).', &
'', &
'RESULT', &
'  The returned value has the same bit sequence as I except the designated bit', &
'  is unconditionally set to 1.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ibset', &
'      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int16) :: i', &
'        ! basic usage', &
'         print *,ibset (12, 1), ''ibset(12,1) has the value 14''', &
'', &
'         ! it is easier to see using binary representation', &
'         i=int(b''0000000000000110'',kind=int16)', &
'         write(*,''(b16.16,1x,i0,1x,i0)'') ibset(i,12), ibset(i,12), i', &
'', &
'        ! elemental', &
'         print *,''an array of initial values may be given as well''', &
'         print *,ibset(i=[0,4096], pos=2)', &
'         print *', &
'         print *,''a list of positions results in multiple returned values''', &
'         print *,''not multiple bits set in one value, as the routine is  ''', &
'         print *,''a scalar function; calling it elementally essentially  ''', &
'         print *,''calls it multiple times.                               ''', &
'         write(*,''(b16.16)'') ibset(i=0, pos=[1,2,3,4])', &
'', &
'         ! both may be arrays if of the same size', &
'', &
'      end program demo_ibset', &
'', &
'  Results:', &
'', &
'       >           14 ibset(12,1) has the value 14', &
'       > 0001000000000110 4102 6', &
'       >  an array of initial values may be given as well', &
'       >            4        4100', &
'       >', &
'       >  a list of positions results in multiple returned values', &
'       >  not multiple bits set in one value, as the routine is', &
'       >  a scalar function; calling it elementally essentially', &
'       >  calls it multiple times.', &
'       > 0000000000000010', &
'       > 0000000000000100', &
'       > 0000000000001000', &
'       > 0000000000010000', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IBCLR(3)', &
'', &
'  IEOR(3), NOT(3), BTEST(3), IBITS(3), IAND(3), IOR(3), IEOR(3), MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               ibset(3fortran)', &
'']

shortname="ibset"
call process()


case('101','ichar')

textblock=[character(len=256) :: &
'', &
'ichar(3fortran)                                                ichar(3fortran)', &
'', &
'NAME', &
'  ICHAR(3) - [CHARACTER:CONVERSION] Character-to-integer code conversion', &
'  function', &
'', &
'SYNOPSIS', &
'  result = ichar(c [,kind])', &
'', &
'           elemental integer(kind=KIND) function ichar(c,KIND)', &
'', &
'            character(len=1,kind=**),intent(in) :: c', &
'            integer,intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  C is a scalar character', &
'', &
'  o  KIND is a constant integer initialization expression indicating the kind', &
'     parameter of the result.', &
'', &
'  o  The return value is of type integer and of kind KIND. If KIND is absent,', &
'     the return value is of default integer kind.', &
'', &
'DESCRIPTION', &
'  ICHAR(3) returns the code for the character in the system''s native character', &
'  set. The correspondence between characters and their codes is not', &
'  necessarily the same across different Fortran implementations. For example,', &
'  a platform using EBCDIC would return different values than an ASCII', &
'  platform.', &
'', &
'  See IACHAR(3) for specifically working with the ASCII character set.', &
'', &
'OPTIONS', &
'  o  C : The input character to determine the code for. Its value shall be', &
'     that of a character capable of representation in the processor.', &
'', &
'  o  KIND : indicates the kind parameter of the result. If KIND is absent, the', &
'     return value is of default integer kind.', &
'', &
'RESULT', &
'  The code in the system default character set for the character being queried', &
'  is returned.', &
'', &
'  The result is the position of C in the processor collating sequence', &
'  associated with the kind type parameter of C.', &
'', &
'  it is nonnegative and less than n, where n is the number of characters in', &
'  the collating sequence.', &
'', &
'  The kind type parameter of the result shall specify an integer kind that is', &
'  capable of representing n.', &
'', &
'  For any characters C and D capable of representation in the processor, C <=', &
'  D is true if and only if ICHAR (C) <= ICHAR (D) is true and C == D is true', &
'  if and only if ICHAR (C) == ICHAR (D) is true.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ichar', &
'      implicit none', &
'', &
'         write(*,*)ichar([''a'',''z'',''A'',''Z''])', &
'', &
'      end program demo_ichar', &
'', &
'  Results:', &
'', &
'                   97         122          65          90', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument -Fortran 2003', &
'', &
'SEE ALSO', &
'  ACHAR(3), CHAR(3), IACHAR(3)', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'  SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               ichar(3fortran)', &
'']

shortname="ichar"
call process()


case('102','ieor')

textblock=[character(len=256) :: &
'', &
'ieor(3fortran)                                                  ieor(3fortran)', &
'', &
'NAME', &
'  IEOR(3) - [BIT:LOGICAL] Bitwise exclusive OR', &
'', &
'SYNOPSIS', &
'  result = ieor(i, j)', &
'', &
'           elemental integer(kind=**) function ieor(i,j)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  I, J and the result must be of the same integer kind.', &
'', &
'  o  An exception is that one of I and J may be a BOZ literal constant', &
'', &
'DESCRIPTION', &
'  IEOR(3) returns a bitwise exclusive-OR of I and J.', &
'', &
'  An exclusive OR or "exclusive disjunction" is a logical operation that is', &
'  true if and only if its arguments differ. In this case a one-bit and a zero-', &
'  bit substitute for true and false.', &
'', &
'  This is often represented with the notation "XOR", for "eXclusive OR".', &
'', &
'  An alternate way to view the process is that the result has the value', &
'  obtained by combining I and J bit-by-bit according to the following table:', &
'', &
'        >  I | J |IEOR (I, J)', &
'        >  --#---#-----------', &
'        >  1 | 1 |  0', &
'        >  1 | 0 |  1', &
'        >  0 | 1 |  1', &
'        >  0 | 0 |  0', &
'', &
'OPTIONS', &
'  o  I : the first of the two values to XOR', &
'', &
'  o  J : the second of the two values to XOR', &
'', &
'  If either I or J is a boz-literal-constant, it is first converted as if by', &
'  the intrinsic function INT to type integer with the kind type parameter of', &
'  the other.', &
'', &
'RESULT', &
'  If a bit is different at the same location in I and J the corresponding bit', &
'  in the result is 1, otherwise it is 0.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ieor', &
'      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int16) :: i,j', &
'        ! basic usage', &
'         print *,ieor (16, 1), '' ==> ieor(16,1) has the value 17''', &
'', &
'         ! it is easier to see using binary representation', &
'         i=int(b''0000000000111111'',kind=int16)', &
'         j=int(b''0000001111110000'',kind=int16)', &
'         write(*,''(a,b16.16,1x,i0)'')''i=     '',i, i', &
'         write(*,''(a,b16.16,1x,i0)'')''j=     '',j, j', &
'         write(*,''(a,b16.16,1x,i0)'')''result='',ieor(i,j), ieor(i,j)', &
'', &
'        ! elemental', &
'         print *,''arguments may be arrays. If both are arrays they ''', &
'         print *,''must have the same shape.                        ''', &
'         print *,ieor(i=[7,4096,9], j=2)', &
'', &
'         ! both may be arrays if of the same size', &
'', &
'      end program demo_ieor', &
'', &
'  Results:', &
'', &
'       >           17  ==> ieor(16,1) has the value 17', &
'       > i=     0000000000111111 63', &
'       > j=     0000001111110000 1008', &
'       > result=0000001111001111 975', &
'       >  arguments may be arrays. If both are arrays they', &
'       >  must have the same shape.', &
'       >            5        4098          11', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3), IAND(3),', &
'  IOR(3), MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                ieor(3fortran)', &
'']

shortname="ieor"
call process()


case('103','image_index')

textblock=[character(len=256) :: &
'', &
'image_index(3fortran)                                    image_index(3fortran)', &
'', &
'NAME', &
'  IMAGE_INDEX(3) - [COLLECTIVE] Cosubscript to image index conversion', &
'', &
'SYNOPSIS', &
'  result = image_index(coarray, sub)', &
'', &
'CHARACTERISTICS', &
'DESCRIPTION', &
'  IMAGE_INDEX(3) returns the image index belonging to a cosubscript.', &
'', &
'OPTIONS', &
'  o  COARRAY : Coarray of any type.', &
'', &
'  o  SUB : default integer rank-1 array of a size equal to the corank of', &
'     COARRAY.', &
'', &
'RESULT', &
'  Scalar default integer with the value of the image index which corresponds', &
'  to the cosubscripts. For invalid cosubscripts the result is zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo image_index', &
'      implicit none', &
'      integer :: array[2,-1:4,8,*]', &
'         ! Writes  28 (or 0 if there are fewer than 28 images)', &
'         write (*,*) image_index(array, [2,0,3,1])', &
'      end demo image_index', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  THIS_IMAGE(3), NUM_IMAGES(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022         image_index(3fortran)', &
'']

shortname="image_index"
call process()


case('104','index')

textblock=[character(len=256) :: &
'', &
'index(3fortran)                                                index(3fortran)', &
'', &
'NAME', &
'  INDEX(3) - [CHARACTER:SEARCH] Position of a substring within a string', &
'', &
'SYNOPSIS', &
'  result = index( string, substring [,back] [,kind] )', &
'', &
'       elemental integer(kind=KIND) function index(string,substring,back,kind)', &
'', &
'        character(len=*,kind=KIND),intent(in) :: string', &
'        character(len=*,kind=KIND),intent(in) :: substring', &
'        logical(kind=**),intent(in),optional :: back', &
'        integer(kind=**),intent(in),optional :: kind', &
'', &
'CHARACTERISTICS', &
'  o  STRING is a character variable of any kind', &
'', &
'  o  SUBSTRING is a character variable of the same kind as STRING', &
'', &
'  o  BACK is a logical variable of any supported kind', &
'', &
'  o  KIND is a scalar integer constant expression.', &
'', &
'DESCRIPTION', &
'  INDEX(3) returns the position of the start of the leftmost or rightmost', &
'  occurrence of string SUBSTRING in STRING, counting from one. If SUBSTRING is', &
'  not present in STRING, zero is returned.', &
'', &
'OPTIONS', &
'  o  STRING : string to be searched for a match', &
'', &
'  o  SUBSTRING : string to attempt to locate in STRING', &
'', &
'  o  BACK : If the BACK argument is present and true, the return value is the', &
'     start of the rightmost occurrence rather than the leftmost.', &
'', &
'  o  KIND : if KIND is present, the kind type parameter is that specified by', &
'     the value of KIND; otherwise the kind type parameter is that of default', &
'     integer type.', &
'', &
'RESULT', &
'  The result is the starting position of the first substring SUBSTRING found', &
'  in STRING.', &
'', &
'  If the length of SUBSTRING is longer than STRING the result is zero.', &
'', &
'  If the substring is not found the result is zero.', &
'', &
'  If BACK is .true. the greatest starting position is returned (that is, the', &
'  position of the right-most match). Otherwise, the smallest position starting', &
'  a match (ie. the left-most match) is returned.', &
'', &
'  The position returned is measured from the left with the first character of', &
'  STRING being position one.', &
'', &
'  Otherwise, if no match is found zero is returned.', &
'', &
'EXAMPLES', &
'  Example program', &
'', &
'      program demo_index', &
'      implicit none', &
'      character(len=*),parameter :: str=&', &
'         ''Search this string for this expression''', &
'         !1234567890123456789012345678901234567890', &
'         write(*,*)&', &
'            index(str,''this'').eq.8,              &', &
'            ! return value is counted from the left end even if BACK=.TRUE.', &
'            index(str,''this'',back=.true.).eq.24, &', &
'            ! INDEX is case-sensitive', &
'            index(str,''This'').eq.0', &
'      end program demo_index', &
'', &
'  Expected Results:', &
'', &
'   T T T', &
'STANDARD', &
'  FORTRAN 77 , with KIND argument Fortran 2003', &
'', &
'SEE ALSO', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022               index(3fortran)', &
'']

shortname="index"
call process()


case('105','int')

textblock=[character(len=256) :: &
'', &
'int(3fortran)                                                    int(3fortran)', &
'', &
'NAME', &
'  INT(3) - [TYPE:NUMERIC] Truncate towards zero and convert to integer', &
'', &
'SYNOPSIS', &
'  result = int(a [,kind])', &
'', &
'           elemental integer(kind=KIND) function int(a, KIND )', &
'', &
'            TYPE(kind=**),intent(in) :: a', &
'            integer,optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  A shall be of type integer, real, or complex, or a boz-literal-constant.', &
'', &
'  o  KIND shall be a scalar integer constant expression.', &
'', &
'DESCRIPTION', &
'  INT(3) truncates towards zero and return an integer.', &
'', &
'OPTIONS', &
'  o  A : is the value to truncate towards zero', &
'', &
'  o  KIND : indicates the kind parameter of the result. If not present the', &
'     returned type is that of default integer type.', &
'', &
'RESULT', &
'  returns an integer variable applying the following rules:', &
'', &
'  CASE:', &
'', &
'  1.  If A is of type integer, INT(a) = a', &
'', &
'  2.  If A is of type real and |A| < 1, INT(A) equals 0. If |A| >= 1, then', &
'      INT(A) equals the integer whose magnitude does not exceed A and whose', &
'      sign is the same as the sign of A.', &
'', &
'  3.  If A is of type complex, rule 2 is applied to the real part of A.', &
'', &
'  4.  If a is a boz-literal constant, it is treated as an integer with the', &
'      kind specified.', &
'', &
'      The interpretation of a bit sequence whose most significant bit is 1 is', &
'      processor dependent.', &
'', &
'  The result is undefined if it cannot be represented in the specified integer', &
'  type.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_int', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer :: i = 42', &
'      complex :: z = (-3.7, 1.0)', &
'      real :: x=-10.5, y=10.5', &
'', &
'         print *, int(x), int(y)', &
'', &
'         print *, int(i)', &
'', &
'         print *, int(z), int(z,8)', &
'         ! elemental', &
'         print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])', &
'         ! note int(3) truncates towards zero', &
'', &
'         ! CAUTION:', &
'         ! a number bigger than a default integer can represent', &
'         ! produces an incorrect result and is not required to', &
'         ! be detected by the program.', &
'         x=real(huge(0))+1000.0', &
'         print *, int(x),x', &
'         ! using a larger kind', &
'         print *, int(x,kind=int64),x', &
'', &
'         print *, int(&', &
'         & B"111111111111111111111111111111111111111111111111111111111111111",&', &
'         & kind=int64)', &
'         print *, int(O"777777777777777777777",kind=int64)', &
'         print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)', &
'', &
'         ! elemental', &
'         print *', &
'         print *,int([ &', &
'         &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &', &
'         &  0.0,   &', &
'         &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])', &
'', &
'      end program demo_int', &
'', &
'  Results:', &
'', &
'       >          -10   10', &
'       >           42', &
'       >           -3  -3', &
'       >          -10  -10  -10   10   10  10', &
'       >  -2147483648   2.14748467E+09', &
'       >   2147484672   2.14748467E+09', &
'       >   9223372036854775807', &
'       >   9223372036854775807', &
'       >   9223372036854775807', &
'       >', &
'       >  -2          -2          -2          -2          -1', &
'       >  -1           0           0           0           1', &
'       >   1           2           2           2           2', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  AINT(3), ANINT(3), NINT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 int(3fortran)', &
'']

shortname="int"
call process()


case('106','ior')

textblock=[character(len=256) :: &
'', &
'ior(3fortran)                                                    ior(3fortran)', &
'', &
'NAME', &
'  IOR(3) - [BIT:LOGICAL] Bitwise logical inclusive OR', &
'', &
'SYNOPSIS', &
'  result = ior(i, j)', &
'', &
'           elemental integer(kind=KIND) function ior(i,j)', &
'', &
'            integer(kind=KIND ,intent(in) :: i', &
'            integer(kind=KIND ,intent(in) :: j', &
'', &
'CHARACTERISTICS', &
'  o  I, J and the result shall have the same integer type and kind, with the', &
'     exception that one of I or J may be a BOZ constant.', &
'', &
'DESCRIPTION', &
'  IOR(3) returns the bit-wise Boolean inclusive-or of I and J.', &
'', &
'OPTIONS', &
'  o  I : one of the pair of values to compare the bits of', &
'', &
'  o  J : one of the pair of values to compare the bits of', &
'', &
'  If either I or J is a BOZ-literal-constant, it is first converted as if by', &
'  the intrinsic function INT(3) to type integer with the kind type parameter', &
'  of the other.', &
'', &
'RESULT', &
'  The result has the value obtained by combining I and J bit-by-bit according', &
'  to the following table:', &
'', &
'                I   J   IOR (I, J)', &
'                1   1        1', &
'                1   0        1', &
'                0   1        1', &
'                0   0        0', &
'', &
'  Where if the bit is set in either input value, it is set in the result.', &
'  Otherwise the result bit is zero.', &
'', &
'  This is commonly called the "bitwise logical inclusive OR" of the two', &
'  values.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ior', &
'      implicit none', &
'      integer :: i, j, k', &
'         i=53       ! i=00110101 binary (lowest order byte)', &
'         j=45       ! j=00101101 binary (lowest order byte)', &
'         k=ior(i,j) ! k=00111101 binary (lowest order byte), k=61 decimal', &
'         write(*,''(i8,1x,b8.8)'')i,i,j,j,k,k', &
'      end program demo_ior', &
'', &
'  Results:', &
'', &
'               53 00110101', &
'               45 00101101', &
'               61 00111101', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3), IAND(3),', &
'  IEOR(3), MVBITS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 ior(3fortran)', &
'']

shortname="ior"
call process()


case('107','iparity')

textblock=[character(len=256) :: &
'', &
'iparity(3fortran)                                            iparity(3fortran)', &
'', &
'NAME', &
'  IPARITY(3) - [BIT:LOGICAL] Bitwise exclusive OR of array elements', &
'', &
'SYNOPSIS', &
'  result = iparity( array [,mask] ) | iparity( array, dim [,mask] )', &
'', &
'           integer(kind=KIND) function iparity(array, dim, mask )', &
'', &
'            integer(kind=KIND),intent(in) :: array(..)', &
'            logical(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'  o  ARRAY - An integer array.', &
'', &
'     o  DIM - an integer scalar from 1 to the rank of ARRAY', &
'', &
'     o  MASK - logical conformable with ARRAY.', &
'', &
'DESCRIPTION', &
'  IPARITY(3) reduces with bitwise xor (exclusive or) the elements of ARRAY', &
'  along dimension DIM if the corresponding element in MASK is .true..', &
'', &
'OPTIONS', &
'  o  ARRAY : an array of integer values', &
'', &
'  o  DIM a value from 1 to the rank of ARRAY.', &
'', &
'  o  MASK : a logical mask either a scalar or an array of the same shape as', &
'     ARRAY.', &
'', &
'RESULT', &
'  The result is of the same type as ARRAY.', &
'', &
'  If DIM is absent, a scalar with the bitwise xor of all elements in ARRAY is', &
'  returned. Otherwise, an array of rank N-1, where N equals the rank of ARRAY,', &
'  and a shape similar to that of ARRAY with dimension DIM dropped is returned.', &
'', &
'  Case (i): The result of IPARITY (ARRAY) has a value equal to the bitwise', &
'  exclusive OR of all the elements of ARRAY. If ARRAY has size zero the result', &
'  has the value zero.', &
'', &
'  Case (ii): The result of IPARITY (ARRAY, MASK=MASK) has a value equal to', &
'  that of', &
'', &
'                     IPARITY (PACK (ARRAY, MASK)).', &
'', &
'  Case (iii): The result of IPARITY (ARRAY, DIM=DIM [, MASK=MASK]) has a value', &
'  equal to that of IPARITY (ARRAY [, MASK=MASK]) if ARRAY has rank one.', &
'', &
'                 Otherwise, an array of values reduced along the dimension', &
'                 **dim** is returned.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_iparity', &
'      implicit none', &
'      integer, dimension(2) :: a', &
'        a(1) = int(b''00100100'')', &
'        a(2) = int(b''01101010'')', &
'        print ''(b8.8)'', iparity(a)', &
'      end program demo_iparity', &
'', &
'  Results:', &
'', &
'         01001110', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  IANY(3), IALL(3), IEOR(3), PARITY(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022             iparity(3fortran)', &
'']

shortname="iparity"
call process()


case('108','is_contiguous')

textblock=[character(len=256) :: &
'', &
'is_contiguous(3fortran)                                is_contiguous(3fortran)', &
'', &
'NAME', &
'  IS_CONTIGUOUS(3) - [ARRAY:INQUIRY] Test if object is contiguous', &
'', &
'SYNOPSIS', &
'  result = is_contiguous(array)', &
'', &
'           logical function is_contiguous(array)', &
'', &
'            type(TYPE(kind=**)),intent(in) :: array', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  ARRAY may be of any type. It shall be an array or assumed-rank. If it is', &
'     a pointer it shall be associated.', &
'', &
'  o  the result is a default logical scalar', &
'', &
'DESCRIPTION', &
'  IS_CONTIGUOUS(3) returns .true. if and only if an object is contiguous.', &
'', &
'  An object is contiguous if it is', &
'', &
'  o  (1) an object with the CONTIGUOUS attribute,', &
'', &
'  o  (2) a nonpointer whole array that is not assumed-shape,', &
'', &
'  o  (3) an assumed-shape array that is argument associated with an array that', &
'     is contiguous,', &
'', &
'  o  (4) an array allocated by an ALLOCATE statement,', &
'', &
'  o  (5) a pointer associated with a contiguous target, or', &
'', &
'  o  (6) a nonzero-sized array section provided that', &
'', &
'     o  (A) its base object is contiguous,', &
'', &
'     o  (B) it does not have a vector subscript,', &
'', &
'     o  (C) the elements of the section, in array element order, are a subset', &
'        of the base object elements that are consecutive in array element', &
'        order,', &
'', &
'     o  (D) if the array is of type character and a substring-range appears,', &
'        the substring-range specifies all of the characters of the parent-', &
'        string,', &
'', &
'     o  (E) only its final part-ref has nonzero rank, and', &
'', &
'     o  (F) it is not the real or imaginary part of an array of type complex.', &
'', &
'  An object is not contiguous if it is an array subobject, and', &
'', &
'  o  the object has two or more elements,', &
'', &
'  o  the elements of the object in array element order are not consecutive in', &
'     the elements of the base object,', &
'', &
'  o  the object is not of type character with length zero, and', &
'', &
'  o  the object is not of a derived type that has no ultimate components other', &
'     than zero-sized arrays and', &
'', &
'  o  characters with length zero.', &
'', &
'  It is processor-dependent whether any other object is contiguous.', &
'', &
'OPTIONS', &
'  o  ARRAY : An array of any type to be tested for being contiguous. If it is', &
'     a pointer it shall be associated.', &
'', &
'RESULT', &
'  The result has the value .true. if ARRAY is contiguous, and .false.', &
'  otherwise.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_is_contiguous', &
'      implicit none', &
'      intrinsic is_contiguous', &
'      real, DIMENSION (1000, 1000), TARGET :: A', &
'      real, DIMENSION (:, :), POINTER       :: IN, OUT', &
'         IN => A              ! Associate IN with target A', &
'         OUT => A(1:1000:2,:) ! Associate OUT with subset of target A', &
'         !', &
'         write(*,*)''IN is '',IS_CONTIGUOUS(IN)', &
'         write(*,*)''OUT is '',IS_CONTIGUOUS(OUT)', &
'         !', &
'      end program demo_is_contiguous', &
'', &
'  Results:', &
'', &
'          IN is  T', &
'          OUT is  F', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022       is_contiguous(3fortran)', &
'']

shortname="is_contiguous"
call process()


case('109','ishft')

textblock=[character(len=256) :: &
'', &
'ishft(3fortran)                                                ishft(3fortran)', &
'', &
'NAME', &
'  ISHFT(3) - [BIT:SHIFT] Logical shift of bits in an integer', &
'', &
'SYNOPSIS', &
'  result = ishftc( i, shift )', &
'', &
'           elemental integer(kind=KIND) function ishft(i, shift )', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: shift', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer of any kind. the kind for I dictates the kind of the', &
'     returned value.', &
'', &
'  o  SHIFT is an integer of any kind.', &
'', &
'DESCRIPTION', &
'  ISHFT(3) returns a value corresponding to I with all of the bits shifted', &
'  SHIFT places left or right as specified by the sign and magnitude of SHIFT.', &
'', &
'  Bits shifted out from the left end or right end are lost; zeros are shifted', &
'  in from the opposite end.', &
'', &
'OPTIONS', &
'  o  I : The value specifying the pattern of bits to shift', &
'', &
'  o  SHIFT : A value of SHIFT greater than zero corresponds to a left shift, a', &
'     value of zero corresponds to no shift, and a value less than zero', &
'     corresponds to a right shift.', &
'', &
'     If the absolute value of SHIFT is greater than BIT_SIZE(I), the value is', &
'     undefined.', &
'', &
'RESULT', &
'  The result has the value obtained by shifting the bits of I by SHIFT', &
'  positions.', &
'', &
'  1.  If SHIFT is positive, the shift is to the left', &
'', &
'  2.  if SHIFT is negative, the shift is to the right', &
'', &
'  3.  if SHIFT is zero, no shift is performed.', &
'', &
'  Bits shifted out from the left or from the right, as appropriate, are lost.', &
'  Zeros are shifted in from the opposite end.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ishft', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer             :: shift', &
'      character(len=*),parameter :: g=''(b32.32,1x,i0)''', &
'', &
'         write(*,*) ishft(3, 1),'' <== typically should have the value 6''', &
'', &
'         shift=4', &
'         write(*,g) ishft(huge(0),shift), shift', &
'         shift=0', &
'         write(*,g) ishft(huge(0),shift), shift', &
'         shift=-4', &
'         write(*,g) ishft(huge(0),shift), shift', &
'      end program demo_ishft', &
'', &
'  Results:', &
'', &
'      >              6  <== typically should have the value 6', &
'      >   11111111111111111111111111110000 4', &
'      >   01111111111111111111111111111111 0', &
'      >   00000111111111111111111111111111 -4', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ISHFTC(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               ishft(3fortran)', &
'']

shortname="ishft"
call process()


case('110','ishftc')

textblock=[character(len=256) :: &
'', &
'ishftc(3fortran)                                              ishftc(3fortran)', &
'', &
'NAME', &
'  ISHFTC(3) - [BIT:SHIFT] Shift rightmost bits circularly, AKA. a logical', &
'  shift', &
'', &
'SYNOPSIS', &
'  result = ishftc( i, shift [,size] )', &
'', &
'           elemental integer(kind=KIND) function ishftc(i, shift, size)', &
'', &
'            integer(kind=KIND),intent(in)        :: i', &
'            integer(kind=**),intent(in)          :: shift', &
'            integer(kind=**),intent(in),optional :: size', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I may be an integer of any kind', &
'', &
'  o  SHIFT and SIZE may be integers of any kind', &
'', &
'  o  the kind for I dictates the kind of the returned value.', &
'', &
'DESCRIPTION', &
'  ISHFTC(3) circularly shifts just the specified rightmost bits of an integer.', &
'', &
'  ISHFTC(3) returns a value corresponding to I with the rightmost SIZE bits', &
'  shifted circularly SHIFT places; that is, bits shifted out one end of the', &
'  section are shifted into the opposite end of the section.', &
'', &
'  A value of SHIFT greater than zero corresponds to a left shift, a value of', &
'  zero corresponds to no shift, and a value less than zero corresponds to a', &
'  right shift.', &
'', &
'OPTIONS', &
'  o  I : The value specifying the pattern of bits to shift', &
'', &
'  o  SHIFT : If SHIFT is positive, the shift is to the left; if SHIFT is', &
'     negative, the shift is to the right; and if SHIFT is zero, no shift is', &
'     performed.', &
'', &
'     The absolute value of SHIFT must be less than SIZE (simply put, the', &
'     number of positions to shift must be less than or equal to the number of', &
'     bits specified to be shifted).', &
'', &
'  o  SIZE : The value must be greater than zero and less than or equal to', &
'     BIT_SIZE(i).', &
'', &
'     The default if BIT_SIZE(I) is absent is to circularly shift the entire', &
'     value I.', &
'', &
'RESULT', &
'  The result characteristics (kind, shape, size, rank, ...) are the same as I.', &
'', &
'  The result has the value obtained by shifting the SIZE rightmost bits of I', &
'  circularly by SHIFT positions.', &
'', &
'  No bits are lost.', &
'', &
'  The unshifted bits are unaltered.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_ishftc', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer             :: i', &
'      character(len=*),parameter :: g=''(b32.32,1x,i0)''', &
'        ! basics', &
'         write(*,*) ishftc(3, 1),'' <== typically should have the value 6''', &
'', &
'         print *, ''lets start with this:''', &
'         write(*,''(b32.32)'')huge(0)', &
'         print *, ''shift the value by various amounts, negative and positive''', &
'         do i= -bit_size(0), bit_size(0), 8', &
'            write(*,g) ishftc(huge(0),i), i', &
'         enddo', &
'        print *,''elemental''', &
'        i=huge(0)', &
'        write(*,*)ishftc(i,[2,3,4,5])', &
'        write(*,*)ishftc([2**1,2**3,-2**7],3)', &
'        print *,''note the arrays have to conform when elemental''', &
'        write(*,*)ishftc([2**1,2**3,-2**7],[5,20,0])', &
'', &
'      end program demo_ishftc', &
'', &
'  Results:', &
'', &
'       >            6  <== typically should have the value 6', &
'       >  lets start with this:', &
'       > 01111111111111111111111111111111', &
'       >  shift the value by various amounts, negative and positive', &
'       > 01111111111111111111111111111111 -32', &
'       > 11111111111111111111111101111111 -24', &
'       > 11111111111111110111111111111111 -16', &
'       > 11111111011111111111111111111111 -8', &
'       > 01111111111111111111111111111111 0', &
'       > 11111111111111111111111101111111 8', &
'       > 11111111111111110111111111111111 16', &
'       > 11111111011111111111111111111111 24', &
'       > 01111111111111111111111111111111 32', &
'       >  elemental', &
'       >           -3          -5          -9         -17', &
'       >           16          64       -1017', &
'       >  note the arrays have to conform when elemental', &
'       >           64     8388608        -128', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  ISHFT(3) - Logical shift of bits in an integer', &
'', &
'  o  SHIFTA(3) - Right shift with fill', &
'', &
'  o  SHIFTL(3) - Shift bits left', &
'', &
'  o  SHIFTR(3) - Combined right shift of the bits of two int...', &
'', &
'  o  DSHIFTL(3) - Combined left shift of the bits of two inte...', &
'', &
'  o  DSHIFTR(3) - Combined right shift of the bits of two int...', &
'', &
'  o  CSHIFT(3) - Circular shift elements of an array', &
'', &
'  o  EOSHIFT(3) - End-off shift elements of an array', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              ishftc(3fortran)', &
'']

shortname="ishftc"
call process()


case('111','is_iostat_end')

textblock=[character(len=256) :: &
'', &
'is_iostat_end(3fortran)                                is_iostat_end(3fortran)', &
'', &
'NAME', &
'  IS_IOSTAT_END(3) - [STATE:INQUIRY] Test for end-of-file value', &
'', &
'SYNOPSIS', &
'  result = is_iostat_end(i)', &
'', &
'           elemental logical function is_iostat_end(i)', &
'', &
'            integer,intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I is integer of any kind', &
'', &
'  o  the return value is a default logical', &
'', &
'DESCRIPTION', &
'  IS_IOSTAT_END(3) tests whether a variable (assumed returned as a status from', &
'  an I/O statement) has the "end of file" I/O status value.', &
'', &
'  The function is equivalent to comparing the variable with the IOSTAT_END', &
'  parameter of the intrinsic module ISO_FORTRAN_ENV.', &
'', &
'OPTIONS', &
'  o  I : An integer status value to test if indicating end of file.', &
'', &
'RESULT', &
'  returns .true. if and only ifI has the value which indicates an end of file', &
'  condition for IOSTAT= specifiers, and is .false. otherwise.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_iostat', &
'      implicit none', &
'      real               :: value', &
'      integer            :: ios', &
'      character(len=256) :: message', &
'         write(*,*)''Begin entering numeric values, one per line''', &
'         do', &
'            read(*,*,iostat=ios,iomsg=message)value', &
'            if(ios.eq.0)then', &
'               write(*,*)''VALUE='',value', &
'            elseif( is_iostat_end(ios) ) then', &
'               stop ''end of file. Goodbye!''', &
'            else', &
'               write(*,*)''ERROR:'',ios,trim(message)', &
'               exit', &
'            endif', &
'            !', &
'         enddo', &
'      end program demo_iostat', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022       is_iostat_end(3fortran)', &
'']

shortname="is_iostat_end"
call process()


case('112','is_iostat_eor')

textblock=[character(len=256) :: &
'', &
'is_iostat_eor(3fortran)                                is_iostat_eor(3fortran)', &
'', &
'NAME', &
'  IS_IOSTAT_EOR(3) - [STATE:INQUIRY] Test for end-of-record value', &
'', &
'SYNOPSIS', &
'  result = is_iostat_eor(i)', &
'', &
'           elemental integer function is_iostat_eor(i)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I is integer of any kind', &
'', &
'  o  the return value is a default logical', &
'', &
'DESCRIPTION', &
'  IS_IOSTAT_EOR(3) tests whether a variable has the value of the I/O status', &
'  "end of record". The function is equivalent to comparing the variable with', &
'  the IOSTAT_EOR parameter of the intrinsic module ISO_FORTRAN_ENV.', &
'', &
'OPTIONS', &
'  o  I : The value to test as indicating "end of record".', &
'', &
'RESULT', &
'  Returns .true. if and only if I has the value which indicates an end-of-', &
'  record condition for iostat= specifiers, and is .false.  otherwise.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_is_iostat_eor', &
'      use iso_fortran_env, only : iostat_eor', &
'      implicit none', &
'      integer :: inums(5), lun, ios', &
'', &
'        ! create a test file to read from', &
'         open(newunit=lun, form=''formatted'',status=''scratch'')', &
'         write(lun, ''(a)'') ''10 20 30''', &
'         write(lun, ''(a)'') ''40 50 60 70''', &
'         write(lun, ''(a)'') ''80 90''', &
'         write(lun, ''(a)'') ''100''', &
'         rewind(lun)', &
'', &
'         do', &
'            read(lun, *, iostat=ios) inums', &
'            write(*,*)''iostat='',ios', &
'            if(is_iostat_eor(ios)) then', &
'               stop ''end of record''', &
'            elseif(is_iostat_end(ios)) then', &
'               print *,''end of file''', &
'               exit', &
'            elseif(ios.ne.0)then', &
'               print *,''I/O error'',ios', &
'               exit', &
'            endif', &
'         enddo', &
'', &
'         close(lun,iostat=ios,status=''delete'')', &
'', &
'      end program demo_is_iostat_eor', &
'', &
'  Results:', &
'', &
'       >  iostat=           0', &
'       >  iostat=          -1', &
'       >  end of file', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022       is_iostat_eor(3fortran)', &
'']

shortname="is_iostat_eor"
call process()


case('113','kind')

textblock=[character(len=256) :: &
'', &
'kind(3fortran)                                                  kind(3fortran)', &
'', &
'NAME', &
'  KIND(3) - [KIND:INQUIRY] Query kind of an entity', &
'', &
'SYNOPSIS', &
'  result = kind(x)', &
'', &
'           integer function kind(x)', &
'', &
'            type(TYPE,kind=**),intent(in) :: x(..)', &
'', &
'CHARACTERISTICS', &
'  o  X may be of any intrinsic type. It may be a scalar or an array.', &
'', &
'  o  the result is a default integer scalar', &
'', &
'DESCRIPTION', &
'  KIND(X)(3) returns the kind value of the entity X.', &
'', &
'OPTIONS', &
'  o  X : Value to query the kind of.', &
'', &
'RESULT', &
'  The return value indicates the kind of the argument X.', &
'', &
'  Note that kinds are processor-dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_kind', &
'      implicit none', &
'      integer,parameter :: dc = kind('' '')', &
'      integer,parameter :: dl = kind(.true.)', &
'', &
'         print *, "The default character kind is ", dc', &
'         print *, "The default logical kind is ", dl', &
'', &
'      end program demo_kind', &
'', &
'  Results:', &
'', &
'          The default character kind is            1', &
'          The default logical kind is            4', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  ALLOCATED(3) - Status of an allocatable entity', &
'', &
'  o  IS_CONTIGUOUS(3) - test if object is contiguous', &
'', &
'  o  LBOUND(3) - Lower dimension bounds of an array', &
'', &
'  o  RANK(3) - Rank of a data object', &
'', &
'  o  SHAPE(3) - Determine the shape of an array', &
'', &
'  o  SIZE(3) - Determine the size of an array', &
'', &
'  o  UBOUND(3) - Upper dimension bounds of an array', &
'', &
'  o  BIT_SIZE(3) - Bit size inquiry function', &
'', &
'  o  STORAGE_SIZE(3) - Storage size in bits', &
'', &
'  o  KIND(3) - Kind of an entity', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                kind(3fortran)', &
'']

shortname="kind"
call process()


case('114','lbound')

textblock=[character(len=256) :: &
'', &
'lbound(3fortran)                                              lbound(3fortran)', &
'', &
'NAME', &
'  LBOUND(3) - [ARRAY:INQUIRY] Lower dimension bounds of an array', &
'', &
'SYNOPSIS', &
'  result = lbound(array [,dim] [,kind] )', &
'', &
'           elemental TYPE(kind=KIND) function lbound(array,dim,kind)', &
'', &
'            TYPE(kind=KIND),intent(in)           :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            integer(kind=**),intent(in),optional :: kind', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY shall be assumed-rank or an array, of any type. It cannot be an', &
'     unallocated allocatable array or a pointer that is not associated.', &
'', &
'  o  DIM shall be a scalar integer. The corresponding actual argument shall', &
'     not be an optional dummy argument, a disassociated pointer, or an', &
'     unallocated allocatable.', &
'', &
'  o  KIND an integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'  o  The return value is of type integer and of kind KIND. If KIND is absent,', &
'     the return value is of default integer kind. The result is scalar if DIM', &
'     is present; otherwise, the result is an array of rank one and size n,', &
'     where n is the rank of ARRAY.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  RESULT(3) returns the lower bounds of an array, or a single lower bound', &
'  along the DIM dimension.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array, of any type.', &
'', &
'  o  DIM : Shall be a scalar integer. If DIM is absent, the result is an array', &
'     of the upper bounds of ARRAY.', &
'', &
'  o  KIND : An integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'RESULT', &
'  If DIM is absent, the result is an array of the lower bounds of ARRAY.', &
'', &
'  If DIM is present, the result is a scalar corresponding to the lower bound', &
'  of the array along that dimension. If ARRAY is an expression rather than a', &
'  whole array or array structure component, or if it has a zero extent along', &
'  the relevant dimension, the lower bound is taken to be', &
'', &
'  1.', &
'', &
'      NOTE1', &
'', &
'      If **array** is assumed-rank and has rank zero, **dim** cannot be', &
'      present since it cannot satisfy the requirement **1 <= dim <= 0**.', &
'', &
'EXAMPLES', &
'  Note that in my opinion this function should not be used on assumed-size', &
'  arrays or in any function without an explicit interface. Errors can occur if', &
'  there is no interface defined.', &
'', &
'  Sample program', &
'', &
'      ! program demo_lbound', &
'      module m_bounds', &
'      implicit none', &
'       contains', &
'          subroutine msub(arr)', &
'             !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array', &
'             integer,intent(in) :: arr(:)', &
'             write(*,*)''MSUB: LOWER='',lbound(arr), &', &
'             & ''UPPER='',ubound(arr), &', &
'             & ''SIZE='',size(arr)', &
'          end subroutine msub', &
'       end module m_bounds', &
'', &
'       program demo_lbound', &
'       use m_bounds, only : msub', &
'       implicit none', &
'       interface', &
'          subroutine esub(arr)', &
'          integer,intent(in) :: arr(:)', &
'          end subroutine esub', &
'       end interface', &
'       integer :: arr(-10:10)', &
'          write(*,*)''MAIN: LOWER='',lbound(arr), &', &
'          & ''UPPER='',ubound(arr), &', &
'          & ''SIZE='',size(arr)', &
'          call csub()', &
'          call msub(arr)', &
'          call esub(arr)', &
'       contains', &
'      subroutine csub', &
'         write(*,*)''CSUB: LOWER='',lbound(arr), &', &
'         & ''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'      end subroutine csub', &
'      end', &
'', &
'       subroutine esub(arr)', &
'       implicit none', &
'       integer,intent(in) :: arr(:)', &
'          ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE', &
'          ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)', &
'          write(*,*)''ESUB: LOWER='',lbound(arr), &', &
'          & ''UPPER='',ubound(arr), &', &
'          & ''SIZE='',size(arr)', &
'       end subroutine esub', &
'', &
'      !end program demo_lbound', &
'', &
'  Results:', &
'', &
'         MAIN: LOWER=         -10 UPPER=          10 SIZE=          21', &
'         CSUB: LOWER=         -10 UPPER=          10 SIZE=          21', &
'         MSUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'         ESUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  Array inquiry:', &
'', &
'  o  SIZE(3) - Determine the size of an array', &
'', &
'  o  RANK(3) - Rank of a data object', &
'', &
'  o  SHAPE(3) - Determine the shape of an array', &
'', &
'  o  UBOUND(3) - Upper dimension bounds of an array', &
'', &
'  CO_UBOUND(3), CO_LBOUND(3)', &
'', &
'  State Inquiry:', &
'', &
'  o  ALLOCATED(3) - Status of an allocatable entity', &
'', &
'  o  IS_CONTIGUOUS(3) - Test if object is contiguous', &
'', &
'  Kind Inquiry:', &
'', &
'  o  KIND(3) - Kind of an entity', &
'', &
'  Bit Inquiry:', &
'', &
'  o  STORAGE_SIZE(3) - Storage size in bits', &
'', &
'  o  BIT_SIZE(3) - Bit size inquiry function', &
'', &
'  o  BTEST(3) - Tests a bit of an integer value.', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              lbound(3fortran)', &
'']

shortname="lbound"
call process()


case('115','leadz')

textblock=[character(len=256) :: &
'', &
'leadz(3fortran)                                                leadz(3fortran)', &
'', &
'NAME', &
'  LEADZ(3) - [BIT:COUNT] Number of leading zero bits of an integer', &
'', &
'SYNOPSIS', &
'  result = leadz(i)', &
'', &
'           elemental integer function leadz(i)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I may be an integer of any kind.', &
'', &
'  o  the return value is a default integer type.', &
'', &
'DESCRIPTION', &
'  LEADZ(3) returns the number of leading zero bits of an integer.', &
'', &
'OPTIONS', &
'  o  I : integer to count the leading zero bits of.', &
'', &
'RESULT', &
'  The number of leading zero bits, taking into account the kind of the input', &
'  value. If all the bits of I are zero, the result value is BIT_SIZE(I).', &
'', &
'  The result may also be thought of as BIT_SIZE(I)-1-K where K is the position', &
'  of the leftmost 1 bit in the input I. Positions are from 0 to bit-size(),', &
'  with 0 at the right-most bit.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_leadz', &
'      implicit none', &
'      integer :: value, i', &
'      character(len=80) :: f', &
'', &
'        ! make a format statement for writing a value as a bit string', &
'        write(f,''("(b",i0,".",i0,")")'')bit_size(value),bit_size(value)', &
'', &
'        ! show output for various integer values', &
'        value=0', &
'        do i=-150, 150, 50', &
'           value=i', &
'           write (*,''("LEADING ZERO BITS=",i3)'',advance=''no'') leadz(value)', &
'           write (*,''(" OF VALUE ")'',advance=''no'')', &
'           write(*,f,advance=''no'') value', &
'           write(*,''(*(1x,g0))'') "AKA",value', &
'        enddo', &
'        ! Notes:', &
'        ! for two''s-complements programming environments a negative non-zero', &
'        ! integer value will always start with a 1 and a positive value with 0', &
'        ! as the first bit is the sign bit. Such platforms are very common.', &
'      end program demo_leadz', &
'', &
'  Results:', &
'', &
'        LEADING ZERO BITS=  0 OF VALUE 11111111111111111111111101101010 AKA -150', &
'        LEADING ZERO BITS=  0 OF VALUE 11111111111111111111111110011100 AKA -100', &
'        LEADING ZERO BITS=  0 OF VALUE 11111111111111111111111111001110 AKA -50', &
'        LEADING ZERO BITS= 32 OF VALUE 00000000000000000000000000000000 AKA 0', &
'        LEADING ZERO BITS= 26 OF VALUE 00000000000000000000000000110010 AKA 50', &
'        LEADING ZERO BITS= 25 OF VALUE 00000000000000000000000001100100 AKA 100', &
'        LEADING ZERO BITS= 24 OF VALUE 00000000000000000000000010010110 AKA 150', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BIT_SIZE(3), POPCNT(3), POPPAR(3), TRAILZ(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               leadz(3fortran)', &
'']

shortname="leadz"
call process()


case('116','len')

textblock=[character(len=256) :: &
'', &
'len(3fortran)                                                    len(3fortran)', &
'', &
'NAME', &
'  LEN(3) - [CHARACTER] Length of a character entity', &
'', &
'SYNOPSIS', &
'  result = len(string [,kind])', &
'', &
'           integer(kind=KIND) function len(string,KIND)', &
'', &
'            character(len=*),intent(in) :: string(..)', &
'            integer,optional,intent(in) :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  STRING is a scalar or array character variable', &
'', &
'  o  KIND is a scalar integer constant expression.', &
'', &
'  o  the returned value is the same integer kind as the KIND argument, or of', &
'     the default integer kind if KIND is not specified.', &
'', &
'DESCRIPTION', &
'  LEN(3) returns the length of a character string.', &
'', &
'  If STRING is an array, the length of a single element of STRING is returned,', &
'  as all elements of an array are the same length.', &
'', &
'  Note that STRING need not be defined when this intrinsic is invoked, as only', &
'  the length (not the content) of STRING is needed.', &
'', &
'OPTIONS', &
'  o  STRING : A scalar or array string to return the length of. If it is an', &
'     unallocated allocatable variable or a pointer that is not associated, its', &
'     length type parameter shall not be deferred.', &
'', &
'  o  KIND : A constant indicating the kind parameter of the result.', &
'', &
'RESULT', &
'  The result has a value equal to the number of characters in STRING if it is', &
'  scalar or in an element of STRING if it is an array.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'      program demo_len', &
'      implicit none', &
'', &
'      ! fixed length', &
'      character(len=40) :: string', &
'      ! allocatable length', &
'      character(len=:),allocatable :: astring', &
'      character(len=:),allocatable :: many_strings(:)', &
'      integer :: ii', &
'        ! BASIC USAGE', &
'         ii=len(string)', &
'         write(*,*)''length ='',ii', &
'', &
'        ! ALLOCATABLE VARIABLE LENGTH CAN CHANGE', &
'        ! the allocatable string length will be the length of RHS expression', &
'         astring='' How long is this allocatable string? ''', &
'         write(*,*)astring, '' LEN='', len(astring)', &
'        ! print underline', &
'         write(*,*) repeat(''='',len(astring))', &
'        ! assign new value to astring and length changes', &
'         astring=''New allocatable string''', &
'         write(*,*)astring, '' LEN='', len(astring)', &
'        ! print underline', &
'         write(*,*) repeat(''='',len(astring))', &
'', &
'        ! THE STRING LENGTH WILL BE CONSTANT FOR A FIXED-LENGTH VARIABLE', &
'         string='' How long is this fixed string? ''', &
'         write(*,*)string,'' LEN='',len(string)', &
'         string=''New fixed string ''', &
'         write(*,*)string,'' LEN='',len(string)', &
'', &
'        ! ALL STRINGS IN AN ARRAY ARE THE SAME LENGTH', &
'        ! a scalar is returned for an array, as all values in a Fortran', &
'        ! character array must be of the same length.', &
'         many_strings = [ character(len=7) :: ''Tom'', ''Dick'', ''Harry'' ]', &
'         write(*,*)''length of ALL elements of array='',len(many_strings)', &
'', &
'        ! NAME%LEN IS ESSENTIALLY THE SAME AS LEN(NAME)', &
'        ! you can also query the length (and other attributes) of a string', &
'        ! using a "type parameter inquiry" (available since fortran 2018)', &
'         write(*,*)''length from type parameter inquiry='',string%len', &
'        ! %len is equivalent to a call to LEN() except the kind of the integer', &
'        ! value returned is always of default kind.', &
'', &
'        ! LOOK AT HOW A PASSED STRING CAN BE USED ...', &
'         call passed('' how long? '')', &
'', &
'      contains', &
'', &
'         subroutine passed(str)', &
'         character(len=*),intent(in)  :: str', &
'         ! the length of str can be used in the definitions of variables', &
'            ! you can query the length of the passed variable', &
'            write(*,*)''length of passed value is '', LEN(str)', &
'         end subroutine passed', &
'', &
'      end program demo_len', &
'', &
'  Results:', &
'', &
'       >  length =          40', &
'       >   How long is this allocatable string?  LEN=          38', &
'       >  ======================================', &
'       >  New allocatable string LEN=          22', &
'       >  ======================', &
'       >   How long is this fixed string?          LEN=          40', &
'       >  New fixed string                         LEN=          40', &
'       >  length of ALL elements of array=           7', &
'       >  length from type parameter inquiry=          40', &
'       >  length of passed value is           11', &
'', &
'STANDARD', &
'  FORTRAN 77 ; with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines that', &
'  allow you to deal with leading and trailing blanks.', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 len(3fortran)', &
'']

shortname="len"
call process()


case('117','len_trim')

textblock=[character(len=256) :: &
'', &
'len_trim(3fortran)                                          len_trim(3fortran)', &
'', &
'NAME', &
'  LEN_TRIM(3) - [CHARACTER:WHITESPACE] Character length without trailing blank', &
'  characters', &
'', &
'SYNOPSIS', &
'  result = len_trim(string [,kind])', &
'', &
'         elemental integer(kind=KIND) function len_trim(string,KIND)', &
'', &
'          character(len=*),intent(in) :: string', &
'          integer(kind=KIND),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  STRING is of type character', &
'', &
'  o  KIND is a scalar integer constant expression specifying the kind of the', &
'     returned value.', &
'', &
'  o  The return value is of type integer and of kind KIND. If KIND is absent,', &
'     the return value is of default integer kind.', &
'', &
'DESCRIPTION', &
'  LEN_TRIM(3) returns the length of a character string, ignoring any trailing', &
'  blanks.', &
'', &
'OPTIONS', &
'  o  STRING : The input string whose length is to be measured.', &
'', &
'  o  KIND : Indicates the kind parameter of the result.', &
'', &
'RESULT', &
'  The result equals the number of characters remaining after any trailing', &
'  blanks in STRING are removed.', &
'', &
'  If the input argument is of zero length or all blanks the result is zero.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'      program demo_len_trim', &
'      implicit none', &
'      character(len=:),allocatable :: string', &
'      integer :: i', &
'      ! basic usage', &
'         string=" how long is this string?     "', &
'         write(*,*) string', &
'         write(*,*)''UNTRIMMED LENGTH='',len(string)', &
'         write(*,*)''TRIMMED LENGTH='',len_trim(string)', &
'', &
'         ! print string, then print substring of string', &
'         string=''xxxxx   ''', &
'         write(*,*)string,string,string', &
'         i=len_trim(string)', &
'         write(*,*)string(:i),string(:i),string(:i)', &
'         !', &
'        ! elemental example', &
'         ELE:block', &
'         ! an array of strings may be used', &
'         character(len=:),allocatable :: tablet(:)', &
'         tablet=[character(len=256) :: &', &
'         & '' how long is this string?     '',&', &
'         & ''and this one?'']', &
'            write(*,*)''UNTRIMMED LENGTH=  '',len(tablet)', &
'            write(*,*)''TRIMMED LENGTH=    '',len_trim(tablet)', &
'            write(*,*)''SUM TRIMMED LENGTH='',sum(len_trim(tablet))', &
'         endblock ELE', &
'         !', &
'      end program demo_len_trim', &
'', &
'  Results:', &
'', &
'           how long is this string?', &
'', &
'    UNTRIMMED LENGTH=', &
'      30', &
'', &
'    TRIMMED LENGTH=', &
'      25', &
'', &
'    xxxxx', &
'      xxxxx   xxxxx xxxxxxxxxxxxxxx', &
'', &
'    UNTRIMMED LENGTH=', &
'      256', &
'', &
'    TRIMMED LENGTH=', &
'      25          13', &
'', &
'    SUM TRIMMED LENGTH=', &
'      38', &
'', &
'STANDARD', &
'  Fortran 95 . KIND argument added with Fortran 2003.', &
'', &
'SEE ALSO', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: REPEAT(3), LEN(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022            len_trim(3fortran)', &
'']

shortname="len_trim"
call process()


case('118','lge')

textblock=[character(len=256) :: &
'', &
'lge(3fortran)                                                    lge(3fortran)', &
'', &
'NAME', &
'  LGE(3) - [CHARACTER:COMPARE] ASCII Lexical greater than or equal', &
'', &
'SYNOPSIS', &
'  result = lge(string_a, stringb)', &
'', &
'           elemental logical function lge(string_a, string_b)', &
'', &
'            character(len=*),intent(in) :: string_a', &
'            character(len=*),intent(in) :: string_b', &
'', &
'CHARACTERISTICS', &
'  o  STRING_A is default character or an ASCII character.', &
'', &
'  o  STRING_B is the same type and kind as STRING_A', &
'', &
'  o  the result is a default logical', &
'', &
'DESCRIPTION', &
'  LGE(3) determines whether one string is lexically greater than or equal to', &
'  another string, where the two strings are interpreted as containing ASCII', &
'  character codes. If STRING_A and STRING_B are not the same length, the', &
'  shorter is compared as if spaces were appended to it to form a value that', &
'  has the same length as the longer.', &
'', &
'  The lexical comparison intrinsics LGE(3), LGT(3), LLE(3), and LLT(3) differ', &
'  from the corresponding intrinsic operators .ge., .gt., .le., and .lt., in', &
'  that the latter use the processor''s character ordering (which is not ASCII', &
'  on some targets), whereas the former always use the ASCII ordering.', &
'', &
'OPTIONS', &
'  o  STRING_A : string to be tested', &
'', &
'  o  STRING_B : string to compare to STRING_A', &
'', &
'RESULT', &
'  Returns .true. if string_a == string_b, and .false. otherwise, based on the', &
'  ASCII collating sequence.', &
'', &
'  If both input arguments are null strings, .true. is always returned.', &
'', &
'  If either string contains a character not in the ASCII character set, the', &
'  result is processor dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_lge', &
'      implicit none', &
'      integer :: i', &
'         print *,''the ASCII collating sequence for printable characters''', &
'         write(*,''(1x,19a)'')(char(i),i=32,126) ! ASCII order', &
'         write(*,*) lge(''abc'',''ABC'')           ! [T] lowercase is > uppercase', &
'         write(*,*) lge(''abc'',''abc  '')         ! [T] trailing spaces', &
'         ! If both strings are of zero length the result is true', &
'         write(*,*) lge('''','''')                 ! [T]', &
'         write(*,*) lge('''',''a'')                ! [F] the null string is padded', &
'         write(*,*) lge(''a'','''')                ! [T]', &
'         ! elemental', &
'         write(*,*) lge(''abc'',[''abc'',''123''])   ! [T T]  scalar and array', &
'         write(*,*) lge([''cba'', ''123''],''abc'')  ! [T F]', &
'         write(*,*) lge([''abc'',''123''],[''cba'',''123'']) ! [F T]  both arrays', &
'      end program demo_lge', &
'', &
'  Results:', &
'', &
'       >  the ASCII collating sequence for printable characters', &
'       >   !"#$%&''()*+,-./012', &
'       >  3456789:;<=>?@ABCDE', &
'       >  FGHIJKLMNOPQRSTUVWX', &
'       >  YZ[\]^_`abcdefghijk', &
'       >  lmnopqrstuvwxyz{|}~', &
'       >  T', &
'       >  T', &
'       >  T', &
'       >  F', &
'       >  T', &
'       >  T T', &
'       >  T F', &
'       >  F T', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  LGT(3), LLE(3), LLT(3)', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'  SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 lge(3fortran)', &
'']

shortname="lge"
call process()


case('119','lgt')

textblock=[character(len=256) :: &
'', &
'lgt(3fortran)                                                    lgt(3fortran)', &
'', &
'NAME', &
'  LGT(3) - [CHARACTER:COMPARE] ASCII Lexical greater than', &
'', &
'SYNOPSIS', &
'  result = lgt(string_a, string_b)', &
'', &
'            elemental logical function lgt(string_a, string_b)', &
'', &
'             character(len=*),intent(in) :: string_a', &
'             character(len=*),intent(in) :: string_b', &
'', &
'CHARACTERISTICS', &
'  o  STRING_A is default character or an ASCII character.', &
'', &
'  o  STRING_B is the same type and kind as STRING_A', &
'', &
'  o  the result is a default logical', &
'', &
'DESCRIPTION', &
'  LGT(3) determines whether one string is lexically greater than another', &
'  string, where the two strings are interpreted as containing ASCII character', &
'  codes. If the String A and String B are not the same length, the shorter is', &
'  compared as if spaces were appended to it to form a value that has the same', &
'  length as the longer.', &
'', &
'  In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT differ', &
'  from the corresponding intrinsic operators .ge., .gt., .le., and .lt., in', &
'  that the latter use the processor''s character ordering (which is not ASCII', &
'  on some targets), whereas the former always use the ASCII ordering.', &
'', &
'OPTIONS', &
'  o  STRING_A : string to be tested', &
'', &
'  o  STRING_B : string to compare to STRING_A', &
'', &
'RESULT', &
'  Returns .true. if string_a > string_b, and .false. otherwise, based on the', &
'  ASCII ordering.', &
'', &
'  If both input arguments are null strings, .false. is returned.', &
'', &
'  If either string contains a character not in the ASCII character set, the', &
'  result is processor dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_lgt', &
'      implicit none', &
'      integer :: i', &
'         print *,''the ASCII collating sequence for printable characters''', &
'         write(*,''(1x,19a)'')(char(i),i=32,126)', &
'', &
'         write(*,*) lgt(''abc'',''ABC'')          ! [T] lowercase is > uppercase', &
'         write(*,*) lgt(''abc'',''abc  '')        ! [F] trailing spaces', &
'', &
'         ! If both strings are of zero length the result is false.', &
'         write(*,*) lgt('''','''')                ! [F]', &
'         write(*,*) lgt('''',''a'')               ! [F] the null string is padded', &
'         write(*,*) lgt(''a'','''')               ! [T]', &
'         write(*,*) lgt(''abc'',[''abc'',''123''])  ! [F T]  scalar and array', &
'         write(*,*) lgt([''cba'', ''123''],''abc'') ! [T F]', &
'         write(*,*) lgt([''abc'',''123''],[''cba'',''123'']) ! [F F]  both arrays', &
'      end program demo_lgt', &
'', &
'  Results:', &
'', &
'       >  the ASCII collating sequence for printable characters', &
'       >   !"#$%&''()*+,-./012', &
'       >  3456789:;<=>?@ABCDE', &
'       >  FGHIJKLMNOPQRSTUVWX', &
'       >  YZ[\]^_`abcdefghijk', &
'       >  lmnopqrstuvwxyz{|}~', &
'       >  T', &
'       >  F', &
'       >  F', &
'       >  F', &
'       >  T', &
'       >  F T', &
'       >  T F', &
'       >  F F', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  LGE(3), LLE(3), LLT(3)', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'  SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 lgt(3fortran)', &
'']

shortname="lgt"
call process()


case('120','lle')

textblock=[character(len=256) :: &
'', &
'lle(3fortran)                                                    lle(3fortran)', &
'', &
'NAME', &
'  LLE(3) - [CHARACTER:COMPARE] ASCII Lexical less than or equal', &
'', &
'SYNOPSIS', &
'  result = lle(string_a, stringb)', &
'', &
'            elemental logical function lle(string_a, string_b)', &
'', &
'             character(len=*),intent(in) :: string_a', &
'             character(len=*),intent(in) :: string_b', &
'', &
'CHARACTERISTICS', &
'  o  STRING_A is default character or an ASCII character.', &
'', &
'  o  STRING_B is the same type and kind as STRING_A', &
'', &
'  o  the result is a default logical', &
'', &
'DESCRIPTION', &
'  LLE(3) determines whether one string is lexically less than or equal to', &
'  another string, where the two strings are interpreted as containing ASCII', &
'  character codes.', &
'', &
'  If STRING_A and STRING_B are not the same length, the shorter is compared as', &
'  if spaces were appended to it to form a value that has the same length as', &
'  the longer.', &
'', &
'  Leading spaces are significant.', &
'', &
'  In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT differ', &
'  from the corresponding intrinsic operators .ge., .gt., .le., and .lt., in', &
'  that the latter use the processor''s character ordering (which is not ASCII', &
'  on some targets), whereas LLE(3) always uses the ASCII ordering.', &
'', &
'OPTIONS', &
'  o  STRING_A : string to be tested', &
'', &
'  o  STRING_B : string to compare to STRING_A', &
'', &
'RESULT', &
'  o  RESULT Returns .true. if STRING_A <= STRING_B, and .false.  otherwise,', &
'     based on the ASCII collating sequence.', &
'', &
'     If both input arguments are null strings, .true. is always returned.', &
'', &
'     If either string contains a character not in the ASCII character set, the', &
'     result is processor dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_lle', &
'      implicit none', &
'      integer :: i', &
'         print *,''the ASCII collating sequence for printable characters''', &
'         write(*,''(1x,19a)'')(char(i),i=32,126)', &
'        ! basics', &
'', &
'         print *,''case matters''', &
'         write(*,*) lle(''abc'',''ABC'')          ! F lowercase is > uppercase', &
'', &
'         print *,''a space is the lowest printable character''', &
'         write(*,*) lle(''abcd'',''abc'')         ! F  d > space', &
'         write(*,*) lle(''abc'',''abcd'')         ! T  space < d', &
'', &
'         print *,''leading spaces matter, trailing spaces do not''', &
'         write(*,*) lle(''abc'',''abc  '')        ! T trailing spaces', &
'         write(*,*) lle(''abc'','' abc'')         ! F leading spaces are significant', &
'', &
'         print *,''even null strings are padded and compared''', &
'         ! If both strings are of zero length the result is true.', &
'         write(*,*) lle('''','''')                ! T', &
'         write(*,*) lle('''',''a'')               ! T the null string is padded', &
'         write(*,*) lle(''a'','''')               ! F', &
'         print *,''elemental''', &
'         write(*,*) lle(''abc'',[''abc'',''123''])  ! [T,F] scalar and array', &
'         write(*,*) lle([''cba'', ''123''],''abc'') ! [F,T]', &
'         ! per the rules for elemental procedures arrays must be the same size', &
'         write(*,*) lle([''abc'',''123''],[''cba'',''123'']) ! [T,T] both arrays', &
'      end program demo_lle', &
'', &
'  Results:', &
'', &
'       >  the ASCII collating sequence for printable characters', &
'       >   !"#$%&''()*+,-./012', &
'       >  3456789:;<=>?@ABCDE', &
'       >  FGHIJKLMNOPQRSTUVWX', &
'       >  YZ[\]^_`abcdefghijk', &
'       >  lmnopqrstuvwxyz{|}~', &
'       >  case matters', &
'       >  F', &
'       >  a space is the lowest printable character', &
'       >  F', &
'       >  T', &
'       >  leading spaces matter, trailing spaces do not', &
'       >  T', &
'       >  F', &
'       >  even null strings are padded and compared', &
'       >  T', &
'       >  T', &
'       >  F', &
'       >  elemental', &
'       >  T F', &
'       >  F T', &
'       >  T T', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  LGE(3), LGT(3), LLT(3)', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3),', &
'', &
'  SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 lle(3fortran)', &
'']

shortname="lle"
call process()


case('121','llt')

textblock=[character(len=256) :: &
'', &
'llt(3fortran)                                                    llt(3fortran)', &
'', &
'NAME', &
'  LLT(3) - [CHARACTER:COMPARE] ASCII Lexical less than', &
'', &
'SYNOPSIS', &
'  result = llt(string_a, stringb)', &
'', &
'            elemental logical function llt(string_a, string_b)', &
'', &
'             character(len=*),intent(in) :: string_a', &
'             character(len=*),intent(in) :: string_b', &
'', &
'CHARACTERISTICS', &
'  o  STRING_A is default character or an ASCII character.', &
'', &
'  o  STRING_B is the same type and kind as STRING_A', &
'', &
'  o  the result is a default logical', &
'', &
'DESCRIPTION', &
'  LLT(3) determines whether one string is lexically less than another string,', &
'  where the two strings are interpreted as containing ASCII character codes.', &
'  If the STRING_A and STRING_B are not the same length, the shorter is', &
'  compared as if spaces were appended to it to form a value that has the same', &
'  length as the longer.', &
'', &
'  In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT differ', &
'  from the corresponding intrinsic operators .ge., .gt., .le., and .lt., in', &
'  that the latter use the processor''s character ordering (which is not ASCII', &
'  on some targets), whereas the former always use the ASCII ordering.', &
'', &
'OPTIONS', &
'  o  STRING_A : string to be tested', &
'', &
'  o  STRING_B : string to compare to STRING_A', &
'', &
'RESULT', &
'  Returns .true. if string_a <= string_b, and .false. otherwise, based on the', &
'  ASCII collating sequence.', &
'', &
'  If both input arguments are null strings, .false. is always returned.', &
'', &
'  If either string contains a character not in the ASCII character set, the', &
'  result is processor dependent.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_llt', &
'      implicit none', &
'      integer :: i', &
'', &
'         print *,''the ASCII collating sequence for printable characters''', &
'         write(*,''(1x,19a)'')(char(i),i=32,126) ! ASCII order', &
'', &
'        ! basics', &
'         print *,''case matters''', &
'         write(*,*) llt(''abc'',''ABC'')           ! [F] lowercase is > uppercase', &
'         write(*,*) llt(''abc'',''abc  '')         ! [F] trailing spaces', &
'         ! If both strings are of zero length the result is false.', &
'         write(*,*) llt('''','''')                 ! [F]', &
'         write(*,*) llt('''',''a'')                ! [T] the null string is padded', &
'         write(*,*) llt(''a'','''')                ! [F]', &
'         print *,''elemental''', &
'         write(*,*) llt(''abc'',[''abc'',''123''])   ! [F F]  scalar and array', &
'         write(*,*) llt([''cba'', ''123''],''abc'')  ! [F T]', &
'         write(*,*) llt([''abc'',''123''],[''cba'',''123'']) ! [T F]  both arrays', &
'      end program demo_llt', &
'', &
'  Results:', &
'', &
'       >  the ASCII collating sequence for printable characters', &
'       >   !"#$%&''()*+,-./012', &
'       >  3456789:;<=>?@ABCDE', &
'       >  FGHIJKLMNOPQRSTUVWX', &
'       >  YZ[\]^_`abcdefghijk', &
'       >  lmnopqrstuvwxyz{|}~', &
'       >  case matters', &
'       >  F', &
'       >  F', &
'       >  F', &
'       >  T', &
'       >  F', &
'       >  elemental', &
'       >  F F', &
'       >  F T', &
'       >  T F', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  LGE(3), LGT(3), LLE(3))', &
'', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 llt(3fortran)', &
'']

shortname="llt"
call process()


case('122','log10')

textblock=[character(len=256) :: &
'', &
'log10(3fortran)                                                log10(3fortran)', &
'', &
'NAME', &
'  LOG10(3) - [MATHEMATICS] Base 10 or common logarithm', &
'', &
'SYNOPSIS', &
'  result = log10(x)', &
'', &
'           elemental real(kind=KIND) function log10(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be any kind of real value', &
'', &
'  o  the result is the same type and characteristics as X.', &
'', &
'DESCRIPTION', &
'  LOG10(3) computes the base 10 logarithm of X. This is generally called the', &
'  "common logarithm".', &
'', &
'OPTIONS', &
'  o  X : A real value > 0 to take the log of.', &
'', &
'RESULT', &
'  The logarithm to base 10 of X', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_log10', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 10.0_real64', &
'', &
'         x = log10(x)', &
'         write(*,''(*(g0))'')''log10('',x,'') is '',log10(x)', &
'', &
'         ! elemental', &
'         write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &', &
'                           & 100000.0, 1000000.0, 10000000.0])', &
'', &
'      end program demo_log10', &
'', &
'  Results:', &
'', &
'       > log10(1.000000000000000) is .000000000000000', &
'       >   0.0000000E+00   1.000000       2.000000       3.000000       4.000000', &
'       >    5.000000       6.000000       7.000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022               log10(3fortran)', &
'']

shortname="log10"
call process()


case('123','log')

textblock=[character(len=256) :: &
'', &
'log(3fortran)                                                    log(3fortran)', &
'', &
'NAME', &
'  LOG(3) - [MATHEMATICS] Natural logarithm', &
'', &
'SYNOPSIS', &
'  result = log(x)', &
'', &
'         elemental TYPE(kind=KIND) function log(x)', &
'', &
'          TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be any real or complex kind.', &
'', &
'  o  the result is the same type and characteristics as X.', &
'', &
'DESCRIPTION', &
'  LOG(3) computes the natural logarithm of X, i.e. the logarithm to the base', &
'  "e".', &
'', &
'OPTIONS', &
'  o  X : The value to compute the natural log of. If X is real, its value', &
'     shall be greater than zero. If X is complex, its value shall not be zero.', &
'', &
'RESULT', &
'  The natural logarithm of X. If X is the complex value (R,I) , the imaginary', &
'  part "i" is in the range', &
'', &
'          -PI < i <= PI', &
'', &
'  If the real part of X is less than zero and the imaginary part of X is zero,', &
'  then the imaginary part of the result is approximately PI if the imaginary', &
'  part of PI is positive real zero or the processor does not distinguish', &
'  between positive and negative real zero, and approximately -PI if the', &
'  imaginary part of X is negative real zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_log', &
'      implicit none', &
'        real(kind(0.0d0)) :: x = 2.71828182845904518d0', &
'        complex :: z = (1.0, 2.0)', &
'        write(*,*)x, log(x)    ! will yield (approximately) 1', &
'        write(*,*)z, log(z)', &
'      end program demo_log', &
'', &
'  Results:', &
'', &
'            2.7182818284590451        1.0000000000000000', &
'  (1.00000000,2.00000000) (0.804718971,1.10714877)', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 log(3fortran)', &
'']

shortname="log"
call process()


case('124','log_gamma')

textblock=[character(len=256) :: &
'', &
'log_gamma(3fortran)                                        log_gamma(3fortran)', &
'', &
'NAME', &
'  LOG_GAMMA(3) - [MATHEMATICS] Logarithm of the absolute value of the Gamma', &
'  function', &
'', &
'SYNOPSIS', &
'  result = log_gamma(x)', &
'', &
'           elemental real(kind=KIND) function log_gamma(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be any real type', &
'', &
'  o  the return value is of same type and kind as X.', &
'', &
'DESCRIPTION', &
'  LOG_GAMMA(3) computes the natural logarithm of the absolute value of the', &
'  Gamma function.', &
'', &
'OPTIONS', &
'  o  X : neither negative nor zero value to render the result for.', &
'', &
'RESULT', &
'  The result has a value equal to a processor-dependent approximation to the', &
'  natural logarithm of the absolute value of the gamma function of X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_log_gamma', &
'      implicit none', &
'      real :: x = 1.0', &
'         write(*,*)x,log_gamma(x) ! returns 0.0', &
'         write(*,*)x,log_gamma(3.0) ! returns 0.693 (approximately)', &
'      end program demo_log_gamma', &
'', &
'  Results:', &
'', &
'       >    1.000000      0.0000000E+00', &
'       >    1.000000      0.6931472', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  Gamma function: GAMMA(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           log_gamma(3fortran)', &
'']

shortname="log_gamma"
call process()


case('125','logical')

textblock=[character(len=256) :: &
'', &
'logical(3fortran)                                            logical(3fortran)', &
'', &
'NAME', &
'  LOGICAL(3) - [TYPE:LOGICAL] Conversion between kinds of logical values', &
'', &
'SYNOPSIS', &
'  result = logical(l [,kind])', &
'', &
'           elemental logical(kind=KIND) function logical(l,KIND)', &
'', &
'            logical(kind=**),intent(in) :: l', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  L is of type logical', &
'', &
'  o  KIND shall be a scalar integer constant expression. If KIND is present,', &
'     the kind type parameter of the result is that specified by the value of', &
'     KIND; otherwise, the kind type parameter is that of default logical.', &
'', &
'DESCRIPTION', &
'  LOGICAL(3) converts one kind of logical variable to another.', &
'', &
'OPTIONS', &
'  o  L : The logical value to produce a copy of with kind KIND', &
'', &
'  o  KIND : indicates the kind parameter of the result. If not present, the', &
'     default kind is returned.', &
'', &
'RESULT', &
'  The return value is a logical value equal to L, with a kind corresponding to', &
'  KIND, or of the default logical kind if KIND is not given.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      Linux', &
'      program demo_logical', &
'      ! Access array containing the kind type parameter values supported by this', &
'      ! compiler for entities of logical type', &
'      use iso_fortran_env, only : logical_kinds', &
'      implicit none', &
'      integer :: i', &
'', &
'         ! list kind values supported on this platform, which generally vary', &
'         ! in storage size as alias declarations', &
'         do i =1, size(logical_kinds)', &
'            write(*,''(*(g0))'')''integer,parameter :: boolean'', &', &
'            & logical_kinds(i),''='', logical_kinds(i)', &
'         enddo', &
'', &
'      end program demo_logical', &
'', &
'  Results:', &
'', &
'       > integer,parameter :: boolean1=1', &
'       > integer,parameter :: boolean2=2', &
'       > integer,parameter :: boolean4=4', &
'       > integer,parameter :: boolean8=8', &
'       > integer,parameter :: boolean16=16', &
'', &
'STANDARD', &
'  Fortran 95 , related ISO_FORTRAN_ENV module - fortran 2009', &
'', &
'SEE ALSO', &
'  INT(3), REAL(3), CMPLX(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             logical(3fortran)', &
'']

shortname="logical"
call process()


case('126','maskl')

textblock=[character(len=256) :: &
'', &
'maskl(3fortran)                                                maskl(3fortran)', &
'', &
'NAME', &
'  MASKL(3) - [BIT:SET] Generates a left justified mask', &
'', &
'SYNOPSIS', &
'  result = maskl( i [,kind] )', &
'', &
'           elemental integer(kind=KIND) function maskl(i,KIND)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer', &
'', &
'  o  KIND Shall be a scalar constant expression of type integer whose value is', &
'     a supported integer kind.', &
'', &
'  o  The result is an integer of the same kind as I unless KIND is present,', &
'     which is then used to specify the kind of the result.', &
'', &
'DESCRIPTION', &
'  MASKL(3) has its leftmost I bits set to 1, and the remaining bits set to', &
'', &
'  0.', &
'', &
'OPTIONS', &
'  o  I : the number of left-most bits to set in the integer result. It must be', &
'     from 0 to the number of bits for the kind of the result. The default kind', &
'     of the result is the same as I unless the result size is specified by', &
'     KIND. That is, these Fortran statements must be .true. :', &
'', &
'         i >= 0 .and. i < bitsize(i) ! if KIND is not specified', &
'         i >= 0 .and. i < bitsize(0_KIND) ! if KIND is specified', &
'', &
'  o  KIND : designates the kind of the integer result.', &
'', &
'RESULT', &
'  The leftmost I bits of the output integer are set to 1 and the other bits', &
'  are set to 0.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_maskl', &
'      implicit none', &
'      integer :: i', &
'        ! basics', &
'         i=3', &
'         write(*,''(i0,1x,b0)'') i, maskl(i)', &
'', &
'        ! elemental', &
'         write(*,''(*(i11,1x,b0.32,1x,/))'') maskl([(i,i,i=0,bit_size(0),4)])', &
'      end program demo_maskl', &
'', &
'  Results:', &
'', &
'       > 3 11100000000000000000000000000000', &
'       >           0 00000000000000000000000000000000', &
'       >  -268435456 11110000000000000000000000000000', &
'       >   -16777216 11111111000000000000000000000000', &
'       >    -1048576 11111111111100000000000000000000', &
'       >      -65536 11111111111111110000000000000000', &
'       >       -4096 11111111111111111111000000000000', &
'       >        -256 11111111111111111111111100000000', &
'       >         -16 11111111111111111111111111110000', &
'       >          -1 11111111111111111111111111111111', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  MASKR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               maskl(3fortran)', &
'']

shortname="maskl"
call process()


case('127','maskr')

textblock=[character(len=256) :: &
'', &
'maskr(3fortran)                                                maskr(3fortran)', &
'', &
'NAME', &
'  MASKR(3) - [BIT:SET] Generates a right-justified mask', &
'', &
'SYNOPSIS', &
'  result = maskr( i [,kind] )', &
'', &
'           elemental integer(kind=KIND) function maskr(i,KIND)', &
'', &
'            integer(kind=**),intent(in) :: i', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer', &
'', &
'  o  KIND Shall be a scalar constant expression of type integer whose value is', &
'     a supported integer kind.', &
'', &
'  o  The result is an integer of the same kind as I unless KIND is present,', &
'     which is then used to specify the kind of the result.', &
'', &
'DESCRIPTION', &
'  MASKR(3) generates an integer with its rightmost I bits set to 1, and the', &
'  remaining bits set to 0.', &
'', &
'OPTIONS', &
'  o  I : the number of right-most bits to set in the integer result. It must', &
'     be from 0 to the number of bits for the kind of the result. The default', &
'     kind of the result is the same as I unless the result size is specified', &
'     by KIND. That is, these Fortran statements must be .true. :', &
'', &
'         i >= 0 .and. i < bitsize(i) ! if KIND is not specified', &
'         i >= 0 .and. i < bitsize(0_KIND) ! if KIND is specified', &
'', &
'  o  KIND : designates the kind of the integer result.', &
'', &
'RESULT', &
'  The rightmost I bits of the output integer are set to 1 and the other bits', &
'  are set to 0.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_maskr', &
'      implicit none', &
'      integer :: i', &
'', &
'        ! basics', &
'         print *,''basics''', &
'         write(*,''(i0,t5,b32.32)'') 1, maskr(1)', &
'         write(*,''(i0,t5,b32.32)'') 5,  maskr(5)', &
'         write(*,''(i0,t5,b32.32)'') 11, maskr(11)', &
'         print *,"should be equivalent on two''s-complement processors"', &
'         write(*,''(i0,t5,b32.32)'') 1,  shiftr(-1,bit_size(0)-1)', &
'         write(*,''(i0,t5,b32.32)'') 5,  shiftr(-1,bit_size(0)-5)', &
'         write(*,''(i0,t5,b32.32)'') 11, shiftr(-1,bit_size(0)-11)', &
'', &
'        ! elemental', &
'         print *,''elemental ''', &
'         print *,''(array argument accepted like called with each element)''', &
'         write(*,''(*(i11,1x,b0.32,1x,/))'') maskr([(i,i,i=0,bit_size(0),4)])', &
'', &
'      end program demo_maskr', &
'', &
'  Results:', &
'', &
'       >   basics', &
'       >  1   00000000000000000000000000000001', &
'       >  5   00000000000000000000000000011111', &
'       >  11  00000000000000000000011111111111', &
'       >   should be equivalent on two''s-complement processors', &
'       >  1   00000000000000000000000000000001', &
'       >  5   00000000000000000000000000011111', &
'       >  11  00000000000000000000011111111111', &
'       >   elemental', &
'       >   (array argument accepted like called with each element)', &
'       >            0 00000000000000000000000000000000', &
'       >           15 00000000000000000000000000001111', &
'       >          255 00000000000000000000000011111111', &
'       >         4095 00000000000000000000111111111111', &
'       >        65535 00000000000000001111111111111111', &
'       >      1048575 00000000000011111111111111111111', &
'       >     16777215 00000000111111111111111111111111', &
'       >    268435455 00001111111111111111111111111111', &
'       >           -1 11111111111111111111111111111111', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  MASKL(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               maskr(3fortran)', &
'']

shortname="maskr"
call process()


case('128','matmul')

textblock=[character(len=256) :: &
'', &
'matmul(3fortran)                                              matmul(3fortran)', &
'', &
'NAME', &
'  MATMUL(3) - [TRANSFORMATIONAL] Numeric or logical matrix multiplication', &
'', &
'SYNOPSIS', &
'  result = matmul(matrix_a,matrix_b)', &
'', &
'           function matmul(matrix_a, matrix_b)', &
'', &
'            type(TYPE1(kind=**)       :: matrix_a(..)', &
'            type(TYPE2(kind=**)       :: matrix_b(..)', &
'            type(TYPE(kind=PROMOTED)) :: matmul(..)', &
'', &
'CHARACTERISTICS', &
'  o  MATRIX_A is a numeric (integer, real, or complex ) or logical array of', &
'     rank one two.', &
'', &
'  o  MATRIX_B is a numeric (integer, real, or complex ) or logical array of', &
'     rank one two.', &
'', &
'  o  At least one argument must be rank two.', &
'', &
'  o  the size of the first dimension of MATRIX_B must equal the size of the', &
'     last dimension of MATRIX_A.', &
'', &
'  o  the type of the result is the same as if an element of each argument had', &
'     been multiplied as a RHS expression (that is, if the arguments are not of', &
'     the same type the result follows the same rules of promotion as a simple', &
'     scalar multiplication of the two types would produce)', &
'', &
'  o  If one argument is logical, both must be logical. For logicals the', &
'     resulting type is as if the .and. operator has been used on elements from', &
'     the arrays.', &
'', &
'  o  The shape of the result depends on the shapes of the arguments as', &
'     described below.', &
'', &
'DESCRIPTION', &
'  MATMUL(3) performs a matrix multiplication on numeric or logical arguments.', &
'', &
'OPTIONS', &
'  o  MATRIX_A : A numeric or logical array with a rank of one or two.', &
'', &
'  o  MATRIX_B : A numeric or logical array with a rank of one or two. The last', &
'     dimension of MATRIX_A and the first dimension of MATRIX_B must be equal.', &
'', &
'     Note that MATRIX_A and MATRIX_B may be different numeric types.', &
'', &
'RESULT', &
'NUMERIC ARGUMENTS', &
'  If MATRIX_A and MATRIX_B are numeric the result is an array containing the', &
'  conventional matrix product of MATRIX_A and MATRIX_B.', &
'', &
'  First, for the numeric expression C=MATMUL(A,B)', &
'', &
'  o  Any vector A(N) is treated as a row vector A(1,N).', &
'', &
'  o  Any vector B(N) is treated as a column vector B(N,1).', &
'', &
'SHAPE AND RANK', &
'  The shape of the result can then be determined as the number of rows of the', &
'  first matrix and the number of columns of the second; but if any argument is', &
'  of rank one (a vector) the result is also rank one.  Conversely when both', &
'  arguments are of rank two, the result has a rank of two. That is ...', &
'', &
'  o  If MATRIX_A has shape [n,m] and MATRIX_B has shape [m,k], the result has', &
'     shape [n,k].', &
'', &
'  o  If MATRIX_A has shape [m] and MATRIX_B has shape [m,k], the result has', &
'     shape [k].', &
'', &
'  o  If MATRIX_A has shape [n,m] and MATRIX_B has shape [m], the result has', &
'     shape [n].', &
'', &
'VALUES', &
'  Then element C(I,J) of the product is obtained by multiplying term-by-term', &
'  the entries of the ith row of A and the jth column of B, and summing these', &
'  products. In other words, C(I,J) is the dot product of the ith row of A and', &
'  the jth column of B.', &
'', &
'LOGICAL ARGUMENTS', &
'VALUES', &
'  If MATRIX_A and MATRIX_B are of type logical, the array elements of the', &
'  result are instead:', &
'', &
'        Value_of_Element (i,j) = &', &
'        ANY( (row_i_of_MATRIX_A) .AND. (column_j_of_MATRIX_B) )', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_matmul', &
'      implicit none', &
'      integer :: a(2,3), b(3,2), c(2), d(3), e(2,2), f(3), g(2), v1(4),v2(4)', &
'         a = reshape([1, 2, 3, 4, 5, 6], [2, 3])', &
'         b = reshape([10, 20, 30, 40, 50, 60], [3, 2])', &
'         c = [1, 2]', &
'         d = [1, 2, 3]', &
'         e = matmul(a, b)', &
'         f = matmul(c,a)', &
'         g = matmul(a,d)', &
'', &
'         call print_matrix_int(''A is '',a)', &
'         call print_matrix_int(''B is '',b)', &
'         call print_vector_int(''C is '',c)', &
'         call print_vector_int(''D is '',d)', &
'         call print_matrix_int(''E is matmul(A,B)'',e)', &
'         call print_vector_int(''F is matmul(C,A)'',f)', &
'         call print_vector_int(''G is matmul(A,D)'',g)', &
'', &
'         ! look at argument shapes when one is a vector', &
'         write(*,''(" > shape")'')', &
'         ! at least one argument must be of rank two', &
'         ! so for two vectors at least one must be reshaped', &
'         v1=[11,22,33,44]', &
'         v2=[10,20,30,40]', &
'', &
'         ! these return a vector C(1:1)', &
'         ! treat A(1:n) as A(1:1,1:n)', &
'         call print_vector_int(''Cd is a vector (not a scalar)'',&', &
'         & matmul(reshape(v1,[1,size(v1)]),v2))', &
'         ! or treat B(1:m) as B(1:m,1:1)', &
'         call print_vector_int(''cD is a vector too'',&', &
'         & matmul(v1,reshape(v2,[size(v2),1])))', &
'', &
'         ! or treat A(1:n) as A(1:1,1:n) and B(1:m) as B(1:m,1:1)', &
'         ! but note this returns a matrix C(1:1,1:1) not a vector!', &
'         call print_matrix_int(''CD is a matrix'',matmul(&', &
'         & reshape(v1,[1,size(v1)]), &', &
'         & reshape(v2,[size(v2),1])))', &
'', &
'      contains', &
'', &
'      ! CONVENIENCE ROUTINES TO PRINT IN ROW-COLUMN ORDER', &
'      subroutine print_vector_int(title,arr)', &
'      character(len=*),intent(in)  :: title', &
'      integer,intent(in)           :: arr(:)', &
'         call print_matrix_int(title,reshape(arr,[1,shape(arr)]))', &
'      end subroutine print_vector_int', &
'', &
'      subroutine print_matrix_int(title,arr)', &
'      !@(#) print small 2d integer arrays in row-column format', &
'      character(len=*),parameter :: all=''(" > ",*(g0,1x))'' ! a handy format', &
'      character(len=*),intent(in)  :: title', &
'      integer,intent(in)           :: arr(:,:)', &
'      integer                      :: i', &
'      character(len=:),allocatable :: biggest', &
'', &
'         print all', &
'         print all, trim(title)', &
'         biggest=''           ''  ! make buffer to write integer into', &
'         ! find how many characters to use for integers', &
'         write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'         ! use this format to write a row', &
'         biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'         ! print one row of array at a time', &
'         do i=1,size(arr,dim=1)', &
'            write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'            write(*,''(" ]")'')', &
'         enddo', &
'', &
'      end subroutine print_matrix_int', &
'', &
'      end program demo_matmul', &
'', &
'  Results:', &
'', &
'          >', &
'          > A is', &
'          > [  1,  3,  5 ]', &
'          > [  2,  4,  6 ]', &
'          >', &
'          > B is', &
'          > [  10,  40 ]', &
'          > [  20,  50 ]', &
'          > [  30,  60 ]', &
'          >', &
'          > C is', &
'          > [  1,  2 ]', &
'          >', &
'          > D is', &
'          > [  1,  2,  3 ]', &
'          >', &
'          > E is matmul(A,B)', &
'          > [  220,  490 ]', &
'          > [  280,  640 ]', &
'          >', &
'          > F is matmul(C,A)', &
'          > [   5,  11,  17 ]', &
'          >', &
'          > G is matmul(A,D)', &
'          > [  22,  28 ]', &
'          > shape', &
'          >', &
'          > Cd is a vector (not a scalar)', &
'          > [  3300 ]', &
'          >', &
'          > cD is a vector too', &
'          > [  3300 ]', &
'          >', &
'          > CD is a matrix', &
'          > [  3300 ]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  PRODUCT(3), TRANSPOSE(3)', &
'', &
'RESOURCES', &
'  o  Matrix multiplication : Wikipedia', &
'', &
'  o  The Winograd variant of Strassen''s matrix-matrix multiply algorithm may', &
'     be of interest for optimizing multiplication of very large matrices. See', &
'', &
'          "GEMMW: A portable level 3 BLAS Winograd variant of Strassen''s', &
'          matrix-matrix multiply algorithm",', &
'', &
'          Douglas, C. C., Heroux, M., Slishman, G., and Smith, R. M.,', &
'          Journal of Computational Physics,', &
'          Vol. 110, No. 1, January 1994, pages 1-10.', &
'', &
'  The numerical instabilities of Strassen''s method for matrix multiplication', &
'  requires special processing.', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              matmul(3fortran)', &
'']

shortname="matmul"
call process()


case('129','max')

textblock=[character(len=256) :: &
'', &
'max(3fortran)                                                    max(3fortran)', &
'', &
'NAME', &
'  MAX(3) - [NUMERIC] Maximum value of an argument list', &
'', &
'SYNOPSIS', &
'  result = max(a1, a2, a3, ...)', &
'', &
'           elemental TYPE(kind=KIND) function max(a1, a2, a3, ... )', &
'', &
'            TYPE(kind=KIND,intent(in),optional :: a1', &
'            TYPE(kind=KIND,intent(in),optional :: a2', &
'            TYPE(kind=KIND,intent(in),optional :: a3', &
'                      :', &
'                      :', &
'                      :', &
'', &
'CHARACTERISTICS', &
'  o  A3, A3, A4, ... must be of the same type and kind as A1', &
'', &
'  o  the arguments may (all) be integer, real or character', &
'', &
'  o  there must be at least two arguments', &
'', &
'  o  the length of a character result is the length of the longest argument', &
'', &
'  o  the type and kind of the result is the same as those of the arguments', &
'', &
'DESCRIPTION', &
'  MAX(3) returns the argument with the largest (most positive) value.', &
'', &
'  For arguments of character type, the result is as if the arguments had been', &
'  successively compared with the intrinsic operational operators, taking into', &
'  account the collating sequence of the character kind.', &
'', &
'  If the selected character argument is shorter than the longest argument, the', &
'  result is as all values were extended with blanks on the right to the length', &
'  of the longest argument.', &
'', &
'  It is unusual for a Fortran intrinsic to take an arbitrary number of', &
'  options, and in addition MAX(3) is elemental, meaning any number of', &
'  arguments may be arrays as long as they are of the same shape. The examples', &
'  have an extended description clarifying the resulting behavior for those not', &
'  familiar with calling a "scalar" function elementally with arrays.', &
'', &
'  See maxval(3) for simply getting the max value of an array.', &
'', &
'OPTIONS', &
'  o  A1 : The first argument determines the type and kind of the returned', &
'     value, and of any remaining arguments as well as being a member of the', &
'     set of values to find the maximum (most positive) value of.', &
'', &
'  o  A2,A3,... : the remaining arguments of which to find the maximum value(s)', &
'     of. : There must be at least two arguments to MAX(3).', &
'', &
'RESULT', &
'  The return value corresponds to an array of the same shape of any array', &
'  argument, or a scalar if all arguments are scalar.', &
'', &
'  The returned value when any argument is an array will be an array of the', &
'  same shape where each element is the maximum value occurring at that', &
'  location, treating all the scalar values as arrays of that same shape with', &
'  all elements set to the scalar value.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'      program demo_max', &
'      implicit none', &
'      real :: arr1(4)= [10.0,11.0,30.0,-100.0]', &
'      real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]', &
'      integer :: box(3,4)= reshape([-6,-5,-4,-3,-2,-1,1,2,3,4,5,6],shape(box))', &
'', &
'        ! basic usage', &
'         ! this is simple enough when all arguments are scalar', &
'', &
'         ! the most positive value is returned, not the one with the', &
'         ! largest magnitude', &
'         write(*,*)''scalars:'',max(10.0,11.0,30.0,-100.0)', &
'         write(*,*)''scalars:'',max(-22222.0,-0.0001)', &
'', &
'         ! strings do not need to be of the same length', &
'         write(*,*)''characters:'',max(''the'',''words'',''order'')', &
'', &
'         ! leading spaces are significant; everyone is padded on the right', &
'         ! to the length of the longest argument', &
'         write(*,*)''characters:'',max(''c'',''bb'',''a'')', &
'         write(*,*)''characters:'',max('' c'',''b'',''a'')', &
'', &
'        ! elemental', &
'         ! there must be at least two arguments, so even if A1 is an array', &
'         ! max(A1) is not valid. See MAXVAL(3) and/or MAXLOC(3) instead.', &
'', &
'         ! strings in a single array do need to be of the same length', &
'         ! but the different objects can still be of different lengths.', &
'         write(*,"(*(''""'',a,''""'':,1x))")MAX([''A'',''Z''],[''BB'',''Y ''])', &
'         ! note the result is now an array with the max of every element', &
'         ! position, as can be illustrated numerically as well:', &
'         write(*,''(a,*(i3,1x))'')''box=   '',box', &
'         write(*,''(a,*(i3,1x))'')''box**2='',sign(1,box)*box**2', &
'         write(*,''(a,*(i3,1x))'')''max    '',max(box,sign(1,box)*box**2)', &
'', &
'         ! Remember if any argument is an array by the definition of an', &
'         ! elemental function all the array arguments must be the same shape.', &
'', &
'         ! to find the single largest value of arrays you could use something', &
'         ! like MAXVAL([arr1, arr2]) or probably better (no large temp array),', &
'         ! max(maxval(arr1),maxval(arr2)) instead', &
'', &
'         ! so this returns an array of the same shape as any input array', &
'         ! where each result is the maximum that occurs at that position.', &
'         write(*,*)max(arr1,arr2(1:4))', &
'         ! this returns an array just like arr1 except all values less than', &
'         ! zero are set to zero:', &
'         write(*,*)max(box,0)', &
'         ! When mixing arrays and scalars you can think of the scalars', &
'         ! as being a copy of one of the arrays with all values set to', &
'         ! the scalar value.', &
'', &
'      end program demo_max', &
'', &
'  Results:', &
'', &
'          scalars:   30.00000', &
'          scalars: -9.9999997E-05', &
'          characters:words', &
'          characters:c', &
'          characters:b', &
'  "BB" "Z "', &
'', &
'    box=', &
'      -6  -5  -4  -3  -2  -1   1   2   3   4   5   6', &
'', &
'    box**2=-36 -25 -16', &
'      -9  -4  -1   1   4   9  16  25  36', &
'', &
'    max', &
'      -6  -5  -4  -3  -2  -1   1   4   9  16  25  36', &
'', &
'    20.00000', &
'      21.00000  32.00000  -100.0000', &
'', &
'    0 0  0  0  0  0', &
'', &
'    1 2  3  4  5  6', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  MAXLOC(3), MINLOC(3), MAXVAL(3), MINVAL(3), MIN(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 max(3fortran)', &
'']

shortname="max"
call process()


case('130','maxexponent')

textblock=[character(len=256) :: &
'', &
'maxexponent(3fortran)                                    maxexponent(3fortran)', &
'', &
'NAME', &
'  MAXEXPONENT(3) - [NUMERIC MODEL] Maximum exponent of a real kind', &
'', &
'SYNOPSIS', &
'  result = maxexponent(x)', &
'', &
'           elemental integer function maxexponent(x)', &
'', &
'            real(kind=**),intent(in)   :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is a real scalar or array of any real kind', &
'', &
'  o  the result is a default integer scalar', &
'', &
'DESCRIPTION', &
'  MAXEXPONENT(3) returns the maximum exponent in the model of the type of X.', &
'', &
'OPTIONS', &
'  o  X : A value used to select the kind of real to return a value for.', &
'', &
'RESULT', &
'  The value returned is the maximum exponent for the kind of the value queried', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_maxexponent', &
'      use, intrinsic :: iso_fortran_env, only : real32,real64,real128', &
'      implicit none', &
'      character(len=*),parameter :: g=''(*(g0,1x))''', &
'         print  g,  minexponent(0.0_real32),   maxexponent(0.0_real32)', &
'         print  g,  minexponent(0.0_real64),   maxexponent(0.0_real64)', &
'         print  g,  minexponent(0.0_real128),  maxexponent(0.0_real128)', &
'      end program demo_maxexponent', &
'', &
'  Results:', &
'', &
'         -125 128', &
'         -1021 1024', &
'         -16381 16384', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MINEXPONENT(3),', &
'  NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022         maxexponent(3fortran)', &
'']

shortname="maxexponent"
call process()


case('131','maxloc')

textblock=[character(len=256) :: &
'', &
'maxloc(3fortran)                                              maxloc(3fortran)', &
'', &
'NAME', &
'  MAXLOC(3) - [ARRAY:LOCATION] Location of the maximum value within an array', &
'', &
'SYNOPSIS', &
'  result = maxloc(array [,mask]) | maxloc(array [,dim] [,mask])', &
'', &
'           NUMERIC function maxloc(array, dim, mask)', &
'', &
'            NUMERIC,intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  NUMERIC designates any intrinsic numeric type and kind.', &
'', &
'DESCRIPTION', &
'  MAXLOC(3) determines the location of the element in the array with the', &
'  maximum value, or, if the DIM argument is supplied, determines the locations', &
'  of the maximum element along each row of the array in the DIM direction.', &
'', &
'  If MASK is present, only the elements for which MASK is .true. are', &
'  considered. If more than one element in the array has the maximum value, the', &
'  location returned is that of the first such element in array element order.', &
'', &
'  If the array has zero size, or all of the elements of MASK are .false., then', &
'  the result is an array of zeroes. Similarly, if DIM is supplied and all of', &
'  the elements of MASK along a given row are zero, the result value for that', &
'  row is zero.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array of type integer, real, or character.', &
'', &
'  o  DIM : (Optional) Shall be a scalar of type integer, with a value between', &
'     one and the rank of ARRAY, inclusive. It may not be an optional dummy', &
'     argument.', &
'', &
'  o  MASK : Shall be an array of type logical, and conformable with ARRAY.', &
'', &
'RESULT', &
'  If DIM is absent, the result is a rank-one array with a length equal to the', &
'  rank of ARRAY. If DIM is present, the result is an array with a rank one', &
'  less than the rank of ARRAY, and a size corresponding to the size of ARRAY', &
'  with the DIM dimension removed. If DIM is present and ARRAY has a rank of', &
'  one, the result is a scalar. In all cases, the result is of default integer', &
'  type.', &
'', &
'  The value returned is reference to the offset from the beginning of the', &
'  array, not necessarily the subscript value if the array subscripts do not', &
'  start with one.', &
'', &
'EXAMPLES', &
'  sample program', &
'', &
'      program demo_maxloc', &
'      implicit none', &
'      integer      :: ii', &
'      integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]', &
'      integer,save :: ints(3,5)= reshape([&', &
'         1,  2,  3,  4,  5, &', &
'         10, 20, 30, 40, 50, &', &
'         11, 22, 33, 44, 55  &', &
'      ],shape(ints),order=[2,1])', &
'', &
'          write(*,*) maxloc(ints)', &
'          write(*,*) maxloc(ints,dim=1)', &
'          write(*,*) maxloc(ints,dim=2)', &
'          ! when array bounds do not start with one remember MAXLOC(3) returns', &
'          ! the offset relative to the lower bound-1 of the location of the', &
'          ! maximum value, not the subscript of the maximum value. When the', &
'          ! lower bound of the array is one, these values are the same. In', &
'          ! other words, MAXLOC(3) returns the subscript of the value assuming', &
'          ! the first subscript of the array is one no matter what the lower', &
'          ! bound of the subscript actually is.', &
'          write(*,''(g0,1x,g0)'') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))', &
'          write(*,*)maxloc(i)', &
'', &
'      end program demo_maxloc', &
'', &
'  Results:', &
'', &
'       >     3       5', &
'       >     3       3       3       3       3', &
'       >     5       5       5', &
'       >  -3 47', &
'       >  -2 48', &
'       >  -1 49', &
'       >  0 50', &
'       >  1 49', &
'       >  2 48', &
'       >  3 47', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  FINDLOC(3) - Location of first element of ARRAY identified by MASK along', &
'     dimension DIM matching a target', &
'', &
'  o  MINLOC(3) - Location of the minimum value within an array', &
'', &
'  o  MAXVAL(3)', &
'', &
'  o  MINVAL(3)', &
'', &
'  o  MAX(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              maxloc(3fortran)', &
'']

shortname="maxloc"
call process()


case('132','maxval')

textblock=[character(len=256) :: &
'', &
'maxval(3fortran)                                              maxval(3fortran)', &
'', &
'NAME', &
'  MAXVAL(3) - [ARRAY:REDUCTION] Determines the maximum value in an array or', &
'  row', &
'', &
'SYNOPSIS', &
'  result = maxval(array [,mask]) | maxval(array [,dim] [,mask])', &
'', &
'           NUMERIC function maxval(array ,dim, mask)', &
'', &
'            NUMERIC,intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  NUMERIC designates any numeric type and kind.', &
'', &
'DESCRIPTION', &
'  MAXVAL(3) determines the maximum value of the elements in an array value,', &
'  or, if the DIM argument is supplied, determines the maximum value along each', &
'  row of the array in the DIM direction. If MASK is present, only the elements', &
'  for which MASK is .true. are considered. If the array has zero size, or all', &
'  of the elements of MASK are .false., then the result is the most negative', &
'  number of the type and kind of ARRAY if ARRAY is numeric, or a string of', &
'  nulls if ARRAY is of character type.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array of type integer, real, or character.', &
'', &
'  o  DIM : (Optional) Shall be a scalar of type integer, with a value between', &
'     one and the rank of ARRAY, inclusive. It may not be an optional dummy', &
'     argument.', &
'', &
'  o  MASK : (Optional) Shall be an array of type logical, and conformable with', &
'     ARRAY.', &
'', &
'RESULT', &
'  If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.  If', &
'  DIM is present, the result is an array with a rank one less than the rank of', &
'  ARRAY, and a size corresponding to the size of ARRAY with the DIM dimension', &
'  removed. In all cases, the result is of the same type and kind as ARRAY.', &
'', &
'EXAMPLES', &
'  sample program:', &
'', &
'      program demo_maxval', &
'      implicit none', &
'      integer,save :: ints(3,5)= reshape([&', &
'         1,  2,  3,  4,  5, &', &
'        10, 20, 30, 40, 50, &', &
'        11, 22, 33, 44, 55  &', &
'      ],shape(ints),order=[2,1])', &
'', &
'         write(*,*) maxval(ints)', &
'         write(*,*) maxval(ints,dim=1)', &
'         write(*,*) maxval(ints,dim=2)', &
'         ! find biggest number less than 30 with mask', &
'         write(*,*) maxval(ints,mask=ints.lt.30)', &
'      end program demo_maxval', &
'', &
'  Results:', &
'', &
'       >  55', &
'       >  11     22     33     44     55', &
'       >   5     50     55', &
'       >  22', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  MAXLOC(3), MINLOC(3), MINVAL(3), MAX(3), MIN(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              maxval(3fortran)', &
'']

shortname="maxval"
call process()


case('133','merge')

textblock=[character(len=256) :: &
'', &
'merge(3fortran)                                                merge(3fortran)', &
'', &
'NAME', &
'  MERGE(3) - [ARRAY:CONSTRUCTION] Merge variables', &
'', &
'SYNOPSIS', &
'  result = merge(tsource, fsource, mask)', &
'', &
'           elemental type(TYPE(kind=KIND)) function merge(tsource,fsource,mask)', &
'', &
'            type(TYPE(kind=KIND)),intent(in) :: tsource', &
'            type(TYPE(kind=KIND)),intent(in) :: fsource', &
'            logical(kind=**),intent(in)   :: mask', &
'            mask** : Shall be of type logical.', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  TSOURCE May be of any type, including user-defined.', &
'', &
'  o  FSOURCE Shall be of the same type and type parameters as TSOURCE.', &
'', &
'  o  MASK shall be of type logical.', &
'', &
'  o  The result will by of the same type and type parameters as TSOURCE.', &
'', &
'DESCRIPTION', &
'  The elemental function MERGE(3) selects values from two arrays or scalars', &
'  according to a logical mask. The result is equal to an element of TSOURCE', &
'  where the corresponding element of MASK is .true., or an element of FSOURCE', &
'  when it is .false. .', &
'', &
'  Multi-dimensional arrays are supported.', &
'', &
'  Note that argument expressions to MERGE(3) are not required to be short-', &
'  circuited so (as an example) if the array X contains zero values in the', &
'  statement below the standard does not prevent floating point divide by zero', &
'  being generated; as 1.0/X may be evaluated for all values of X before the', &
'  mask is used to select which value to retain:', &
'', &
'            y = merge( 1.0/x, 0.0, x /= 0.0 )', &
'', &
'  Note the compiler is also free to short-circuit or to generate an infinity', &
'  so this may work in many programming environments but is not recommended.', &
'', &
'  For cases like this one may instead use masked assignment via the WHERE', &
'  construct:', &
'', &
'            where(x .ne. 0.0)', &
'               y = 1.0/x', &
'            elsewhere', &
'               y = 0.0', &
'            endwhere', &
'', &
'  instead of the more obscure', &
'', &
'            merge(1.0/merge(x,1.0,x /= 0.0), 0.0, x /= 0.0)', &
'', &
'OPTIONS', &
'  o  TSOURCE : May be of any type, including user-defined.', &
'', &
'  o  FSOURCE : Shall be of the same type and type parameters as TSOURCE.', &
'', &
'  o  MASK : Shall be of type logical.', &
'', &
'  Note that (currently) character values must be of the same length.', &
'', &
'RESULT', &
'  The result is built from an element of TSOURCE if MASK is .true. and from', &
'  FSOURCE otherwise.', &
'', &
'  Because TSOURCE and FSOURCE are required to have the same type and type', &
'  parameters (for both the declared and dynamic types), the result is', &
'  polymorphic if and only if both TSOURCE and FSOURCE are polymorphic.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_merge', &
'      implicit none', &
'      integer :: tvals(2,3), fvals(2,3), answer(2,3)', &
'      logical :: mask(2,3)', &
'      integer :: i', &
'      integer :: k', &
'      logical :: chooseleft', &
'', &
'         ! Works with scalars', &
'         k=5', &
'         write(*,*)merge (1.0, 0.0, k > 0)', &
'         k=-2', &
'         write(*,*)merge (1.0, 0.0, k > 0)', &
'', &
'         ! set up some simple arrays that all conform to the', &
'         ! same shape', &
'         tvals(1,:)=[  10, -60,  50 ]', &
'         tvals(2,:)=[ -20,  40, -60 ]', &
'', &
'         fvals(1,:)=[ 0, 3, 2 ]', &
'         fvals(2,:)=[ 7, 4, 8 ]', &
'', &
'         mask(1,:)=[ .true.,  .false., .true. ]', &
'         mask(2,:)=[ .false., .false., .true. ]', &
'', &
'         ! lets use the mask of specific values', &
'         write(*,*)''mask of logicals''', &
'         answer=merge( tvals, fvals, mask )', &
'         call printme()', &
'', &
'         ! more typically the mask is an expression', &
'         write(*, *)''highest values''', &
'         answer=merge( tvals, fvals, tvals > fvals )', &
'         call printme()', &
'', &
'         write(*, *)''lowest values''', &
'         answer=merge( tvals, fvals, tvals < fvals )', &
'         call printme()', &
'', &
'         write(*, *)''zero out negative values''', &
'         answer=merge( tvals, 0, tvals < 0)', &
'         call printme()', &
'', &
'         write(*, *)''binary choice''', &
'         chooseleft=.false.', &
'         write(*, ''(3i4)'')merge([1,2,3],[10,20,30],chooseleft)', &
'         chooseleft=.true.', &
'         write(*, ''(3i4)'')merge([1,2,3],[10,20,30],chooseleft)', &
'', &
'      contains', &
'', &
'      subroutine printme()', &
'            write(*, ''(3i4)'')(answer(i, :), i=1, size(answer, dim=1))', &
'      end subroutine printme', &
'', &
'      end program demo_merge', &
'', &
'  Expected Results:', &
'', &
'       >     mask of logicals', &
'       >      10   3  50', &
'       >       7   4 -60', &
'       >     highest values', &
'       >      10   3  50', &
'       >       7  40   8', &
'       >     lowest values', &
'       >       0 -60   2', &
'       >     -20   4 -60', &
'       >     zero out negative values', &
'       >       0 -60   0', &
'       >     -20   0 -60', &
'       >     binary choice', &
'       >      10  20  30', &
'       >       1   2   3', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  PACK(3) packs an array into an array of rank one', &
'', &
'  o  SPREAD(3) is used to add a dimension and replicate data', &
'', &
'  o  UNPACK(3) scatters the elements of a vector', &
'', &
'  o  TRANSPOSE(3) - Transpose an array of rank two', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               merge(3fortran)', &
'']

shortname="merge"
call process()


case('134','merge_bits')

textblock=[character(len=256) :: &
'', &
'merge_bits(3fortran)                                      merge_bits(3fortran)', &
'', &
'NAME', &
'  MERGE_BITS(3) - [BIT:COPY] Merge bits using a mask', &
'', &
'SYNOPSIS', &
'  result = merge_bits(i, j, mask)', &
'', &
'           elemental integer(kind=KIND) function merge_bits(i,j,mask)', &
'', &
'            integer(kind=KIND), intent(in) :: i, j, mask', &
'', &
'CHARACTERISTICS', &
'  o  the result and all input values have the same integer type and KIND with', &
'     the exception that the mask and either I or J may be a BOZ constant.', &
'', &
'DESCRIPTION', &
'  A common graphics operation in Ternary Raster Operations is to combine bits', &
'  from two different sources, generally referred to as bit-blending.', &
'  MERGE_BITS(3) performs a masked bit-blend of I and J using the bits of the', &
'  MASK value to determine which of the input values to copy bits from.', &
'', &
'  Specifically, The k-th bit of the result is equal to the k-th bit of I if', &
'  the k-th bit of MASK is 1; it is equal to the k-th bit of J otherwise (so', &
'  all three input values must have the same number of bits).', &
'', &
'  The resulting value is the same as would result from', &
'', &
'          ior (iand (i, mask),iand (j, not (mask)))', &
'', &
'  An exception to all values being of the same integer type is that I or J', &
'  and/or the mask may be a BOZ constant (A BOZ constant means it is either a', &
'  Binary, Octal, or Hexadecimal literal constant). The BOZ values are', &
'  converted to the integer type of the non-BOZ value(s) as if called by the', &
'  intrinsic function INT() with the kind of the non-BOZ value(s), so the BOZ', &
'  values must be in the range of the type of the result.', &
'', &
'OPTIONS', &
'  o  I : value to select bits from when the associated bit in the mask is', &
'', &
'     1.', &
'', &
'  o  J : value to select bits from when the associated bit in the mask is', &
'', &
'     0.', &
'', &
'  o  MASK : a value whose bits are used as a mask to select bits from I and J', &
'', &
'RESULT', &
'  The bits blended from I and J using the mask MASK.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_merge_bits', &
'      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int16) :: if_one,if_zero,msk', &
'      character(len=*),parameter :: fmt=''(*(g0, 1X))''', &
'', &
'         ! basic usage', &
'         print *,''MERGE_BITS( 5,10,41) should be 3.=>'',merge_bits(5,10,41)', &
'         print *,''MERGE_BITS(13,18,22) should be 4.=>'',merge_bits(13,18,22)', &
'', &
'         ! use some values in base2 illustratively:', &
'         if_one =int(b''1010101010101010'',kind=int16)', &
'         if_zero=int(b''0101010101010101'',kind=int16)', &
'', &
'         msk=int(b''0101010101010101'',kind=int16)', &
'         print ''("should get all zero bits =>",b16.16)'', &', &
'         & merge_bits(if_one,if_zero,msk)', &
'', &
'         msk=int(b''1010101010101010'',kind=int16)', &
'         print ''("should get all ones bits =>",b16.16)'', &', &
'         & merge_bits(if_one,if_zero,msk)', &
'', &
'         ! using BOZ values', &
'         print fmt, &', &
'         & merge_bits(32767_int16,    o''12345'',         32767_int16), &', &
'         & merge_bits(o''12345'', 32767_int16, b''0000000000010101''), &', &
'         & merge_bits(32767_int16,    o''12345'',             z''1234'')', &
'', &
'         ! a do-it-yourself equivalent for comparison and validation', &
'         print fmt, &', &
'         & ior(iand(32767_int16, 32767_int16),                   &', &
'         &   iand(o''12345'', not(32767_int16))),                  &', &
'', &
'         & ior(iand(o''12345'', int(o''12345'', kind=int16)),        &', &
'         &   iand(32767_int16, not(int(o''12345'', kind=int16)))), &', &
'', &
'         & ior(iand(32767_int16, z''1234''),                       &', &
'         &   iand(o''12345'', not(int( z''1234'', kind=int16))))', &
'', &
'      end program demo_merge_bits', &
'', &
'  Results:', &
'', &
'          MERGE_BITS( 5,10,41) should be 3.=>           3', &
'          MERGE_BITS(13,18,22) should be 4.=>           4', &
'  should get all zero bits =>0000000000000000 should get all ones bits', &
'  =>1111111111111111 32767 32751 5877 32767 32767 5877', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022          merge_bits(3fortran)', &
'']

shortname="merge_bits"
call process()


case('135','min')

textblock=[character(len=256) :: &
'', &
'min(3fortran)                                                    min(3fortran)', &
'', &
'NAME', &
'  MIN(3) - [NUMERIC] Minimum value of an argument list', &
'', &
'SYNOPSIS', &
'  result = min(a1, a2, a3, ... )', &
'', &
'           elemental TYPE(kind=KIND) function min(a1, a2, a3, ... )', &
'', &
'            TYPE(kind=KIND,intent(in)   :: a1', &
'            TYPE(kind=KIND,intent(in)   :: a2', &
'            TYPE(kind=KIND,intent(in)   :: a3', &
'                      :', &
'                      :', &
'                      :', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be integer, real or character.', &
'', &
'DESCRIPTION', &
'  MIN(3) returns the argument with the smallest (most negative) value.', &
'', &
'  See MAX(3) for an extended example of the behavior of MIN(3) as and MAX(3).', &
'', &
'OPTIONS', &
'  o  A1 : the first element of the set of values to determine the minimum of.', &
'', &
'  o  A2, A3, ... : An expression of the same type and kind as A1 completing', &
'     the set of values to find the minimum of.', &
'', &
'RESULT', &
'  The return value corresponds to the minimum value among the arguments, and', &
'  has the same type and kind as the first argument.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'      program demo_min', &
'      implicit none', &
'          write(*,*)min(10.0,11.0,30.0,-100.0)', &
'      end program demo_min', &
'', &
'  Results:', &
'', &
'            -100.0000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  MAXLOC(3), MINLOC(3), MINVAL(3), MAX(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 min(3fortran)', &
'']

shortname="min"
call process()


case('136','minexponent')

textblock=[character(len=256) :: &
'', &
'minexponent(3fortran)                                    minexponent(3fortran)', &
'', &
'NAME', &
'  MINEXPONENT(3) - [NUMERIC MODEL] Minimum exponent of a real kind', &
'', &
'SYNOPSIS', &
'  result = minexponent(x)', &
'', &
'           elemental integer function minexponent(x)', &
'', &
'            real(kind=**),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is a real scalar or array of any real kind', &
'', &
'  o  the result is a default integer scalar', &
'', &
'DESCRIPTION', &
'  MINEXPONENT(3) returns the minimum exponent in the model of the type of X.', &
'', &
'OPTIONS', &
'  o  X : A value used to select the kind of real to return a value for.', &
'', &
'RESULT', &
'  The value returned is the maximum exponent for the kind of the value queried', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_minexponent', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'       &real_kinds, real32, real64, real128', &
'      implicit none', &
'      real(kind=real32) :: x', &
'      real(kind=real64) :: y', &
'          print *, minexponent(x), maxexponent(x)', &
'          print *, minexponent(y), maxexponent(y)', &
'      end program demo_minexponent', &
'', &
'  Expected Results:', &
'', &
'              -125         128', &
'', &
'    -1021', &
'      1024', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022         minexponent(3fortran)', &
'']

shortname="minexponent"
call process()


case('137','minloc')

textblock=[character(len=256) :: &
'', &
'minloc(3fortran)                                              minloc(3fortran)', &
'', &
'NAME', &
'  MINLOC(3) - [ARRAY:LOCATION] Location of the minimum value within an array', &
'', &
'SYNOPSIS', &
'  result = minloc(array [,mask]) | minloc(array [,dim] [,mask])', &
'', &
'           NUMERIC function minloc(array, dim, mask)', &
'', &
'            NUMERIC,intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  NUMERIC is any numeric type and kind.', &
'', &
'DESCRIPTION', &
'  MINLOC(3) determines the location of the element in the array with the', &
'  minimum value, or, if the DIM argument is supplied, determines the locations', &
'  of the minimum element along each row of the array in the DIM direction.', &
'', &
'  If MASK is present, only the elements for which MASK is true. are', &
'  considered.', &
'', &
'  If more than one element in the array has the minimum value, the location', &
'  returned is that of the first such element in array element order.', &
'', &
'  If the array has zero size, or all of the elements of MASK are .false., then', &
'  the result is an array of zeroes. Similarly, if DIM is supplied and all of', &
'  the elements of MASK along a given row are zero, the result value for that', &
'  row is zero.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array of type integer, real, or character.', &
'', &
'  o  DIM : (Optional) Shall be a scalar of type integer, with a value between', &
'     one and the rank of ARRAY, inclusive. It may not be an optional dummy', &
'     argument.', &
'', &
'  o  MASK : Shall be an array of type logical, and conformable with ARRAY.', &
'', &
'RESULT', &
'  If DIM is absent, the result is a rank-one array with a length equal to the', &
'  rank of ARRAY. If DIM is present, the result is an array with a rank one', &
'  less than the rank of ARRAY, and a size corresponding to the size of ARRAY', &
'  with the DIM dimension removed. If DIM is present and ARRAY has a rank of', &
'  one, the result is a scalar. In all cases, the result is of default integer', &
'  type.', &
'', &
'EXAMPLES', &
'  sample program:', &
'', &
'      program demo_minloc', &
'      implicit none', &
'      integer,save :: ints(3,5)= reshape([&', &
'         4, 10,  1,  7, 13, &', &
'         9, 15,  6, 12,  3, &', &
'        14,  5, 11,  2,  8  &', &
'      ],shape(ints),order=[2,1])', &
'         write(*,*) minloc(ints)', &
'         write(*,*) minloc(ints,dim=1)', &
'         write(*,*) minloc(ints,dim=2)', &
'         ! where in each column is the smallest number .gt. 10 ?', &
'         write(*,*) minloc(ints,dim=2,mask=ints.gt.10)', &
'         ! a one-dimensional array with dim=1 explicitly listed returns a scalar', &
'         write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar', &
'      end program demo_minloc', &
'', &
'  Results:', &
'', &
'       >        1       3', &
'       >        1       3       1       3       2', &
'       >        3       5       4', &
'       >        5       4       3', &
'       >        7', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  FINDLOC(3) - Location of first element of ARRAY identified by MASK along', &
'     dimension DIM matching a target', &
'', &
'  o  MAXLOC(3) - Location of the maximum value within an array', &
'', &
'  o  MINLOC(3) - Location of the minimum value within an array', &
'', &
'  o  MIN(3)', &
'', &
'  o  MINVAL(3)', &
'', &
'  o  MAXVAL(3)', &
'', &
'  o  MAX(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              minloc(3fortran)', &
'']

shortname="minloc"
call process()


case('138','minval')

textblock=[character(len=256) :: &
'', &
'minval(3fortran)                                              minval(3fortran)', &
'', &
'NAME', &
'  MINVAL(3) - [ARRAY:REDUCTION] Minimum value of an array', &
'', &
'SYNOPSIS', &
'  result = minval(array, [mask]) | minval(array [,dim] [,mask])', &
'', &
'           NUMERIC function minval(array, dim, mask)', &
'', &
'            NUMERIC,intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  NUMERIC is any numeric type and kind.', &
'', &
'DESCRIPTION', &
'  MINVAL(3) determines the minimum value of the elements in an array value,', &
'  or, if the DIM argument is supplied, determines the minimum value along each', &
'  row of the array in the DIM direction.', &
'', &
'  If MASK is present, only the elements for which MASK is .true. are', &
'  considered.', &
'', &
'  If the array has zero size, or all of the elements of MASK are .false., then', &
'  the result is HUGE(ARRAY) if ARRAY is numeric, or a string of CHAR(LEN=255)', &
'  characters if ARRAY is of character type.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array of type integer, real, or character.', &
'', &
'  o  DIM : (Optional) Shall be a scalar of type integer, with a value between', &
'     one and the rank of ARRAY, inclusive. It may not be an optional dummy', &
'     argument.', &
'', &
'  o  MASK : Shall be an array of type logical, and conformable with ARRAY.', &
'', &
'RESULT', &
'  If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.', &
'', &
'  If DIM is present, the result is an array with a rank one less than the rank', &
'  of ARRAY, and a size corresponding to the size of ARRAY with the DIM', &
'  dimension removed. In all cases, the result is of the same type and kind as', &
'  ARRAY.', &
'', &
'EXAMPLES', &
'  sample program:', &
'', &
'      program demo_minval', &
'      implicit none', &
'      integer :: i', &
'      character(len=*),parameter :: g=''(3x,*(g0,1x))''', &
'', &
'      integer,save :: ints(3,5)= reshape([&', &
'             1,  -2,   3,   4,   5,  &', &
'            10,  20, -30,  40,  50,  &', &
'            11,  22,  33, -44,  55  &', &
'      ],shape(ints),order=[2,1])', &
'', &
'      integer,save :: box(3,5,2)', &
'', &
'         box(:,:,1)=ints', &
'         box(:,:,2)=-ints', &
'', &
'         write(*,*)''Given the array''', &
'         write(*,''(1x,*(g4.4,1x))'') &', &
'         & (ints(i,:),new_line(''a''),i=1,size(ints,dim=1))', &
'', &
'         write(*,*)''What is the smallest element in the array?''', &
'         write(*,g) minval(ints),''at <'',minloc(ints),''>''', &
'', &
'         write(*,*)''What is the smallest element in each column?''', &
'         write(*,g) minval(ints,dim=1)', &
'', &
'         write(*,*)''What is the smallest element in each row?''', &
'         write(*,g) minval(ints,dim=2)', &
'', &
'         ! notice the shape of the output has less columns', &
'         ! than the input in this case', &
'         write(*,*)''What is the smallest element in each column,''', &
'         write(*,*)''considering only those elements that are''', &
'         write(*,*)''greater than zero?''', &
'         write(*,g) minval(ints, dim=1, mask = ints > 0)', &
'', &
'         write(*,*)&', &
'         & ''if everything is false a zero-sized array is NOT returned''', &
'         write(*,*) minval(ints, dim=1, mask = .false.)', &
'         write(*,*)''even for a zero-sized input''', &
'         write(*,g) minval([integer ::], dim=1, mask = .false.)', &
'', &
'         write(*,*)''a scalar answer for everything false is huge()''', &
'         write(*,g) minval(ints, mask = .false.)', &
'         write(*,g) minval([integer ::], mask = .false.)', &
'', &
'         write(*,*)''some calls with three dimensions''', &
'         write(*,g) minval(box, mask = .true. )', &
'         write(*,g) minval(box, dim=1, mask = .true. )', &
'', &
'         write(*,g) minval(box, dim=2, mask = .true. )', &
'         write(*,g) ''shape of answer is '', &', &
'         & shape(minval(box, dim=2, mask = .true. ))', &
'', &
'      end program demo_minval', &
'', &
'  Results:', &
'', &
'       > Given the array', &
'       >    1   -2    3    4    5', &
'       >   10   20  -30   40   50', &
'       >   11   22   33  -44   55', &
'       >', &
'       > What is the smallest element in the array?', &
'       >   -44 at < 3 4 >', &
'       > What is the smallest element in each column?', &
'       >   1 -2 -30 -44 5', &
'       > What is the smallest element in each row?', &
'       >   -2 -30 -44', &
'       > What is the smallest element in each column,', &
'       > considering only those elements that are', &
'       > greater than zero?', &
'       >   1 20 3 4 5', &
'       > if everything is false a zero-sized array is NOT returned', &
'       >  2147483647  2147483647  2147483647  2147483647  2147483647', &
'       > even for a zero-sized input', &
'       >   2147483647', &
'       > a scalar answer for everything false is huge()', &
'       >   2147483647', &
'       >   2147483647', &
'       > some calls with three dimensions', &
'       >   -55', &
'       >   1 -2 -30 -44 5 -11 -22 -33 -40 -55', &
'       >   -2 -30 -44 -5 -50 -55', &
'       >   shape of answer is  3 2', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  MIN(3), MINLOC(3) MAXLOC(3), MAXVAL(3), MIN(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              minval(3fortran)', &
'']

shortname="minval"
call process()


case('139','mod')

textblock=[character(len=256) :: &
'', &
'mod(3fortran)                                                    mod(3fortran)', &
'', &
'NAME', &
'  MOD(3) - [NUMERIC] Remainder function', &
'', &
'SYNOPSIS', &
'  result = mod(a, p)', &
'', &
'           elemental type(TYPE(kind=KIND)) function mod(a,p)', &
'', &
'            type(TYPE(kind=KIND),intent(in) :: a', &
'            type(TYPE(kind=KIND),intent(in) :: p', &
'', &
'CHARACTERISTICS', &
'  o  The result and arguments are all of the same type and kind.', &
'', &
'  o  The type may be any kind of real or integer.', &
'', &
'DESCRIPTION', &
'  MOD(3) computes the remainder of the division of A by P.', &
'', &
'  In mathematics, the remainder is the amount "left over" after performing', &
'  some computation. In arithmetic, the remainder is the integer "left over"', &
'  after dividing one integer by another to produce an integer quotient', &
'  (integer division). In algebra of polynomials, the remainder is the', &
'  polynomial "left over" after dividing one polynomial by another. The modulo', &
'  operation is the operation that produces such a remainder when given a', &
'  dividend and divisor.', &
'', &
'  o  (remainder). (2022, October 10). In Wikipedia.', &
'     https://en.wikipedia.org/wiki/Remainder', &
'', &
'OPTIONS', &
'  o  A : The dividend', &
'', &
'  o  P : the divisor (not equal to zero).', &
'', &
'RESULT', &
'  The return value is the result of A - (INT(A/P) * P).', &
'', &
'  As can be seen by the formula the sign of P is canceled out. Therefore the', &
'  returned value always has the sign of A.', &
'', &
'  Of course, the magnitude of the result will be less than the magnitude of P,', &
'  as the result has been reduced by all multiples of P.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_mod', &
'      implicit none', &
'', &
'         ! basics', &
'          print *, mod( -17,  3 ), modulo( -17,  3 )', &
'          print *, mod(  17, -3 ), modulo(  17, -3 )', &
'          print *, mod(  17,  3 ), modulo(  17,  3 )', &
'          print *, mod( -17, -3 ), modulo( -17, -3 )', &
'', &
'          print *, mod(-17.5, 5.2), modulo(-17.5, 5.2)', &
'          print *, mod( 17.5,-5.2), modulo( 17.5,-5.2)', &
'          print *, mod( 17.5, 5.2), modulo( 17.5, 5.2)', &
'          print *, mod(-17.5,-5.2), modulo(-17.5,-5.2)', &
'', &
'        ! with a divisor of 1 the fractional part is returned', &
'          print *, mod(-17.5, 1.0), modulo(-17.5, 1.0)', &
'          print *, mod( 17.5,-1.0), modulo( 17.5,-1.0)', &
'          print *, mod( 17.5, 1.0), modulo( 17.5, 1.0)', &
'          print *, mod(-17.5,-1.0), modulo(-17.5,-1.0)', &
'', &
'      end program demo_mod', &
'', &
'  Results:', &
'', &
'                   -2           1', &
'                    2          -1', &
'                    2           2', &
'                   -2          -2', &
'', &
'    -1.900001', &
'      3.299999', &
'', &
'      1.900001', &
'        -3.299999', &
'', &
'      1.900001', &
'        1.900001', &
'', &
'    -1.900001', &
'      -1.900001', &
'', &
'  -0.5000000', &
'    0.5000000', &
'', &
'    0.5000000', &
'      -0.5000000', &
'', &
'    0.5000000', &
'      0.5000000', &
'', &
'  -0.5000000', &
'    -0.5000000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  MODULO(3) - Modulo function', &
'', &
'  o  AINT(3) - truncate toward zero to a whole real number', &
'', &
'  o  INT(3) - truncate toward zero to a whole integer number', &
'', &
'  o  ANINT(3) - real nearest whole number', &
'', &
'  o  NINT(3) - integer nearest whole number', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 mod(3fortran)', &
'']

shortname="mod"
call process()


case('140','modulo')

textblock=[character(len=256) :: &
'', &
'modulo(3fortran)                                              modulo(3fortran)', &
'', &
'NAME', &
'  MODULO(3) - [NUMERIC] Modulo function', &
'', &
'SYNOPSIS', &
'  result = modulo(a, p)', &
'', &
'           elemental TYPE(kind=KIND) function modulo(a,p)', &
'', &
'            TYPE(kind=KIND),intent(in) :: a', &
'            TYPE(kind=KIND),intent(in) :: p', &
'', &
'CHARACTERISTICS', &
'  o  A may be any kind of real or integer.', &
'', &
'  o  P is the same type and kind as A', &
'', &
'  o  The result and arguments are all of the same type and kind.', &
'', &
'DESCRIPTION', &
'  MODULO(3) computes the A modulo P.', &
'', &
'OPTIONS', &
'  o  A : the value to take the MODULO of', &
'', &
'  o  P : The value to reduce A by till the remainder is <= P. It shall not be', &
'     zero.', &
'', &
'RESULT', &
'  The type and kind of the result are those of the arguments.', &
'', &
'  o  If A and P are of type integer: MODULO(A,P) has the value of A - FLOOR', &
'     (REAL(A) / REAL(P)) * P.', &
'', &
'  o  If A and P are of type real: MODULO(A,P) has the value of A - FLOOR (A /', &
'     P) * P.', &
'', &
'  The returned value has the same sign as P and a magnitude less than the', &
'  magnitude of P.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_modulo', &
'      implicit none', &
'           print *, modulo(17,3)        ! yields 2', &
'           print *, modulo(17.5,5.5)    ! yields 1.0', &
'', &
'           print *, modulo(-17,3)       ! yields 1', &
'           print *, modulo(-17.5,5.5)   ! yields 4.5', &
'', &
'           print *, modulo(17,-3)       ! yields -1', &
'           print *, modulo(17.5,-5.5)   ! yields -4.5', &
'      end program demo_modulo', &
'', &
'  Results:', &
'', &
'       >            2', &
'       >    1.000000', &
'       >            1', &
'       >    4.500000', &
'       >           -1', &
'       >   -4.500000', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  MOD(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022              modulo(3fortran)', &
'']

shortname="modulo"
call process()


case('141','move_alloc')

textblock=[character(len=256) :: &
'', &
'move_alloc(3fortran)                                      move_alloc(3fortran)', &
'', &
'NAME', &
'  MOVE_ALLOC(3) - [MEMORY] Move allocation from one object to another', &
'', &
'SYNOPSIS', &
'  call move_alloc(from, to [,stat] [,errmsg] )', &
'', &
'           subroutine move_alloc(from, to)', &
'', &
'            type(TYPE(kind=**)),intent(inout),allocatable :: from(..)', &
'            type(TYPE(kind=**)),intent(out),allocatable   :: to(..)', &
'            integer(kind=**),intent(out)   :: stat', &
'            character(len=*),intent(inout) :: errmsg', &
'', &
'CHARACTERISTICS', &
'  o  FROM may be of any type and kind.', &
'', &
'  o  TO shall be of the same type, kind and rank as FROM.', &
'', &
'DESCRIPTION', &
'  MOVE_ALLOC(3) moves the allocation from FROM to TO. FROM will become', &
'  deallocated in the process.', &
'', &
'  This is potentially more efficient than other methods of assigning the', &
'  values in FROM to TO and explicitly deallocating FROM, which are for more', &
'  likely to require a temporary object or a copy of the elements of the array.', &
'', &
'OPTIONS', &
'  o  FROM : The data object to be moved to TO and deallocated.', &
'', &
'  o  TO : The destination data object to move the allocated data object FROM', &
'     to. Typically, it is a different shape than FROM.', &
'', &
'  o  STAT : If STAT is present and execution is successful, it is assigned the', &
'     value zero. : If an error condition occurs,', &
'', &
'     o  if **stat** is absent, error termination is initiated;', &
'', &
'     o  otherwise, if **from** is a coarray and the current team contains a', &
'        stopped image, **stat** is assigned the value STAT\_STOPPED\_IMAGE', &
'        from the intrinsic module ISO\_FORTRAN\_ENV;', &
'', &
'     o  otherwise, if **from** is a coarray and the current team contains a', &
'        failed image, and no other error condition occurs, **stat** is', &
'        assigned the value STAT\_FAILED\_IMAGE from the intrinsic module', &
'        ISO\_FORTRAN\_ENV;', &
'', &
'     o  otherwise, **stat** is assigned a processor-dependent positive value', &
'        that differs from that of STAT\_STOPPED\_IMAGE or STAT\_FAILED\_IMAGE.', &
'', &
'  o  ERRMSG : If the ERRMSG argument is present and an error condition occurs,', &
'     it is assigned an explanatory message. If no error condition occurs, the', &
'     definition status and value of ERRMSG are unchanged.', &
'', &
'EXAMPLES', &
'  Basic sample program to allocate a bigger grid', &
'', &
'      program demo_move_alloc', &
'      implicit none', &
'      ! Example to allocate a bigger GRID', &
'      real, allocatable :: grid(:), tempgrid(:)', &
'      integer :: n, i', &
'', &
'         ! initialize small GRID', &
'         n = 3', &
'         allocate (grid(1:n))', &
'         grid = [ (real (i), i=1,n) ]', &
'', &
'         ! initialize TEMPGRID which will be used to replace GRID', &
'         allocate (tempgrid(1:2*n))    ! Allocate bigger grid', &
'         tempgrid(::2)  = grid         ! Distribute values to new locations', &
'         tempgrid(2::2) = grid + 0.5   ! initialize other values', &
'', &
'         ! move TEMPGRID to GRID', &
'         call MOVE_ALLOC (from=tempgrid, to=grid)', &
'', &
'         ! TEMPGRID should no longer be allocated', &
'         ! and GRID should be the size TEMPGRID was', &
'         if (size (grid) /= 2*n .or. allocated (tempgrid)) then', &
'            print *, "Failure in move_alloc!"', &
'         endif', &
'         print *, allocated(grid), allocated(tempgrid)', &
'         print ''(99f8.3)'', grid', &
'      end program demo_move_alloc', &
'', &
'  Results:', &
'', &
'   T F', &
'  1.000', &
'    1.500   2.000   2.500   3.000   3.500', &
'', &
'STANDARD', &
'  Fortran 2003, STAT and ERRMSG options added 2018', &
'', &
'SEE ALSO', &
'  ALLOCATED(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          move_alloc(3fortran)', &
'']

shortname="move_alloc"
call process()


case('142','mvbits')

textblock=[character(len=256) :: &
'', &
'mvbits(3fortran)                                              mvbits(3fortran)', &
'', &
'NAME', &
'  MVBITS(3) - [BIT:COPY] Reproduce bit patterns found in one integer in', &
'  another', &
'', &
'SYNOPSIS', &
'  call mvbits(from, frompos, len, to, topos)', &
'', &
'          elemental subroutine mvbits( from, frompos, len, to, topos )', &
'', &
'           integer(kind=KIND),intent(in)    :: from', &
'           integer(kind=**),intent(in)      :: frompos', &
'           integer(kind=**),intent(in)      :: len', &
'           integer(kind=KIND),intent(inout) :: to', &
'           integer(kind=**),intent(in)      :: topos', &
'', &
'CHARACTERISTICS', &
'  o  FROM is an integer', &
'', &
'  o  FROMPOS is an integer', &
'', &
'  o  LEN is an integer', &
'', &
'  o  TO is an integer of the same kind as FROM.', &
'', &
'  o  TOPOS is an integer', &
'', &
'DESCRIPTION', &
'  MVBITS(3) copies a bit pattern found in a range of adjacent bits in the', &
'  integer FROM to a specified position in another integer TO (which is of the', &
'  same kind as FROM). It otherwise leaves the bits in TO as-is.', &
'', &
'  The bit positions copied must exist within the value of FROM. That is, the', &
'  values of FROMPOS+LEN-1 and TOPOS+LEN-1 must be nonnegative and less than', &
'  BIT_SIZE(from).', &
'', &
'  The bits are numbered 0 to BIT_SIZE(I)-1, from right to left.', &
'', &
'OPTIONS', &
'  o  FROM : An integer to read bits from.', &
'', &
'  o  FROMPOS : FROMPOS is the position of the first bit to copy. It is a', &
'     nonnegative integer value < BIT_SIZE(FROM).', &
'', &
'  o  LEN : A nonnegative integer value that indicates how many bits to copy', &
'     from FROM. It must not specify copying bits past the end of FROM. That', &
'     is, FROMPOS + LEN must be less than or equal to BIT_SIZE(FROM).', &
'', &
'  o  TO : The integer variable to place the copied bits into. It must be of', &
'     the same kind as FROM and may even be the same variable as FROM, or', &
'     associated to it.', &
'', &
'     TO is set by copying the sequence of bits of length LEN, starting at', &
'     position FROMPOS of FROM to position TOPOS of TO. No other bits of TO are', &
'     altered. On return, the LEN bits of TO starting at TOPOS are equal to the', &
'     value that the LEN bits of FROM starting at FROMPOS had on entry.', &
'', &
'  o  TOPOS : A nonnegative integer value indicating the starting location in', &
'     TO to place the specified copy of bits from FROM. TOPOS + LEN must be', &
'     less than or equal to BIT_SIZE(TO).', &
'', &
'EXAMPLES', &
'  Sample program that populates a new 32-bit integer with its bytes in reverse', &
'  order from the input value (ie. changes the Endian of the integer).', &
'', &
'      program demo_mvbits', &
'      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int32) :: intfrom, intto, abcd_int', &
'      character(len=*),parameter :: bits= ''(g0,t30,b32.32)''', &
'      character(len=*),parameter :: fmt= ''(g0,t30,a,t40,b32.32)''', &
'', &
'          intfrom=huge(0)  ! all bits are 1 accept the sign bit', &
'          intto=0          ! all bits are 0', &
'', &
'          !! CHANGE BIT 0', &
'          ! show the value and bit pattern', &
'          write(*,bits)intfrom,intfrom', &
'          write(*,bits)intto,intto', &
'', &
'          ! copy bit 0 from intfrom to intto to show the rightmost bit changes', &
'          !          (from,    frompos, len,    to, topos)', &
'          call mvbits(intfrom,       0,   1, intto,     0) ! change bit 0', &
'          write(*,bits)intto,intto', &
'', &
'          !! COPY PART OF A VALUE TO ITSELF', &
'          ! can copy bit from a value to itself', &
'          call mvbits(intfrom,0,1,intfrom,31)', &
'          write(*,bits)intfrom,intfrom', &
'', &
'          !! MOVING BYTES AT A TIME', &
'          ! make native integer value with bit patterns', &
'          ! that happen to be the same as the beginning of the alphabet', &
'          ! to make it easy to see the bytes are reversed', &
'          abcd_int=transfer(''abcd'',0)', &
'          ! show the value and bit pattern', &
'          write(*,*)''native''', &
'          write(*,fmt)abcd_int,abcd_int,abcd_int', &
'', &
'          ! change endian of the value', &
'          abcd_int=int_swap32(abcd_int)', &
'          ! show the values and their bit pattern', &
'          write(*,*)''non-native''', &
'          write(*,fmt)abcd_int,abcd_int,abcd_int', &
'', &
'       contains', &
'', &
'       pure elemental function int_swap32(intin) result(intout)', &
'       ! Convert a 32 bit integer from big Endian to little Endian,', &
'       ! or conversely from little Endian to big Endian.', &
'       !', &
'       integer(kind=int32), intent(in)  :: intin', &
'       integer(kind=int32) :: intout', &
'          ! copy bytes from input value to new position in output value', &
'          !          (from,  frompos, len,     to, topos)', &
'          call mvbits(intin,       0,   8, intout,    24) ! byte1 to byte4', &
'          call mvbits(intin,       8,   8, intout,    16) ! byte2 to byte3', &
'          call mvbits(intin,      16,   8, intout,     8) ! byte3 to byte2', &
'          call mvbits(intin,      24,   8, intout,     0) ! byte4 to byte1', &
'       end function int_swap32', &
'', &
'       end program demo_mvbits', &
'', &
'  Results:', &
'', &
'         2147483647                   01111111111111111111111111111111', &
'         0                            00000000000000000000000000000000', &
'         1                            00000000000000000000000000000001', &
'         -1                           11111111111111111111111111111111', &
'          native', &
'         1684234849                   abcd      01100100011000110110001001100001', &
'          non-native', &
'         1633837924                   dcba      01100001011000100110001101100100', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IEOR(3), IBCLR(3), NOT(3), BTEST(3), IBCLR(3), IBITS(3), IBSET(3), IAND(3),', &
'  IOR(3), IEOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              mvbits(3fortran)', &
'']

shortname="mvbits"
call process()


case('143','nearest')

textblock=[character(len=256) :: &
'', &
'nearest(3fortran)                                            nearest(3fortran)', &
'', &
'NAME', &
'  NEAREST(3) - [MODEL_COMPONENTS] Nearest representable number', &
'', &
'SYNOPSIS', &
'  result = nearest(x, s)', &
'', &
'           elemental real(kind=KIND) function nearest(x,s)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'            real(kind=**),intent(in) :: s', &
'', &
'CHARACTERISTICS', &
'  o  X may be a real value of any kind.', &
'', &
'  o  S may be a real value of any kind.', &
'', &
'  o  The return value is of the same type and kind as X.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  NEAREST(3) returns the processor-representable number nearest to X in the', &
'  direction indicated by the sign of S.', &
'', &
'OPTIONS', &
'  o  X : the value to find the nearest representable value of', &
'', &
'  o  S : a non-zero value whose sign is used to determine the direction in', &
'     which to search from X to the representable value.', &
'', &
'     If S is positive, NEAREST returns the processor-representable number', &
'     greater than X and nearest to it.', &
'', &
'     If S is negative, NEAREST returns the processor-representable number', &
'     smaller than X and nearest to it.', &
'', &
'RESULT', &
'  The return value is of the same type as X. If S is positive, NEAREST returns', &
'  the processor-representable number greater than X and nearest to it. If S is', &
'  negative, NEAREST returns the processor-representable number smaller than X', &
'  and nearest to it.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_nearest', &
'      implicit none', &
'', &
'         real :: x, y', &
'         x = nearest(42.0, 1.0)', &
'         y = nearest(42.0, -1.0)', &
'         write (*,"(3(g20.15))") x, y, x - y', &
'', &
'      !  write (*,"(3(g20.15))") &', &
'      !   nearest(tiny(0.0),1.0), &', &
'      !   nearest(tiny(0.0),-1.0), &', &
'      !   nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)', &
'', &
'      !  write (*,"(3(g20.15))") &', &
'      !   nearest(huge(0.0),1.0), &', &
'      !   nearest(huge(0.0),-1.0), &', &
'      !   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)', &
'', &
'      end program demo_nearest', &
'', &
'  Results:', &
'', &
'         42.0000038146973    41.9999961853027    .762939453125000E-05', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022             nearest(3fortran)', &
'']

shortname="nearest"
call process()


case('144','new_line')

textblock=[character(len=256) :: &
'', &
'new_line(3fortran)                                          new_line(3fortran)', &
'', &
'NAME', &
'  NEW_LINE(3) - [CHARACTER:INQUIRY] Newline character', &
'', &
'SYNOPSIS', &
'  result = new_line(c)', &
'', &
'           character(len=1,kind=KIND) function new_line(c)', &
'', &
'            character(len=1,kind=KIND),intent(in) :: c(..)', &
'', &
'CHARACTERISTICS', &
'  o  C shall be of type character. It may be a scalar or an array.', &
'', &
'  o  the result is a character scalar of length one with the same kind type', &
'     parameter as C.', &
'', &
'DESCRIPTION', &
'  NEW_LINE(3) returns the newline character.', &
'', &
'  Normally, newlines are generated with regular formatted I/O statements like', &
'  WRITE() and PRINT() when each statement completes:', &
'', &
'         print *, ''x=11''', &
'         print *', &
'         print *, ''y=22''', &
'         end', &
'', &
'  produces: x=11', &
'', &
'      y=22', &
'', &
'      Alternatively, a "/" descriptor in a format is used to generate a', &
'      newline on the output. For example:', &
'      ```fortran', &
'         write(*,''(a,1x,i0,/,a)'') ''x ='',11,''is the answer''', &
'         end', &
'', &
'  produces:', &
'', &
'         x = 11', &
'         is the answer', &
'', &
'  Also, for formatted sequential output if more data is listed on the output', &
'  statement than can be represented by the format statement a newline is', &
'  generated and then the format is reused until the output list is exhausted.', &
'', &
'         write(*,''(a,"=",i0)'') ''x'', 10, ''y'', 20', &
'         end', &
'', &
'  produces', &
'', &
'         x=10', &
'         y=20', &
'', &
'  But there are occasions, particularly when non-advancing I/O or stream I/O', &
'  is being generated (which does not generate a newline at the end of each', &
'  WRITE statement, as normally occurs) where it is preferable to place a', &
'  newline explicitly in the output at specified points.', &
'', &
'  To do so you must make sure you are generating the correct newline', &
'  character, which the techniques above do automatically.', &
'', &
'  The newline character varies between some platforms, and can even depend on', &
'  the encoding (ie. which character set is being used) of the output file. In', &
'  these cases selecting the correct character to output can be determined by', &
'  the NEW_LINE(3) procedure.', &
'', &
'OPTIONS', &
'  o  C : an arbitrary character whose kind is used to decide on the output', &
'     character that represents a newline.', &
'', &
'RESULT', &
'  Case (i) : If A is default character and the character in position 10 of the', &
'  ASCII collating sequence is representable in the default character set, then', &
'  the result is ACHAR(10).', &
'', &
'  This is the typical case, and just requires using "new_line(''a'')".', &
'', &
'  Case (ii) : If A is an ASCII character or an ISO 10646 character, then the', &
'  result is CHAR(10, KIND (A)).', &
'', &
'  Case (iii) : Otherwise, the result is a processor-dependent character that', &
'  represents a newline in output to files connected for formatted stream', &
'  output if there is such a character.', &
'', &
'  Case (iv) : If not of the previous cases apply, the result is the blank', &
'  character.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_new_line', &
'      implicit none', &
'      character,parameter :: nl=new_line(''a'')', &
'      character(len=:),allocatable :: string', &
'      real :: r', &
'      integer :: i, count', &
'', &
'        ! basics', &
'         ! print a string with a newline embedded in it', &
'         string=''This is record 1.''//nl//''This is record 2.''', &
'         write(*,''(a)'') string', &
'', &
'         ! print a newline character string', &
'         write(*,''(*(a))'',advance=''no'') &', &
'            nl,''This is record 1.'',nl,''This is record 2.'',nl', &
'', &
'         ! output a number of words of random length as a paragraph', &
'         ! by inserting a new_line before line exceeds 70 characters', &
'', &
'        ! simplistic paragraph print using non-advancing I/O', &
'         count=0', &
'         do i=1,100', &
'', &
'            ! make some fake word of random length', &
'            call random_number(r)', &
'            string=repeat(''x'',int(r*10)+1)', &
'', &
'            count=count+len(string)+1', &
'            if(count.gt.70)then', &
'               write(*,''(a)'',advance=''no'')nl', &
'               count=len(string)+1', &
'            endif', &
'            write(*,''(1x,a)'',advance=''no'')string', &
'         enddo', &
'         write(*,''(a)'',advance=''no'')nl', &
'', &
'      end program demo_new_line', &
'', &
'  Results:', &
'', &
'         This is record 1.', &
'         This is record 2.', &
'', &
'         This is record 1.', &
'         This is record 2.', &
'          x x xxxx xxxxxxx xxxxxxxxxx xxxxxxxxx xxxx xxxxxxxxxx xxxxxxxx', &
'          xxxxxxxxx xxxx xxxxxxxxx x xxxxxxxxx xxxxxxxx xxxxxxxx xxxx x', &
'          xxxxxxxxxx x x x xxxxxx xxxxxxxxxx x xxxxxxxxxx x xxxxxxx xxxxxxxxx', &
'          xx xxxxxxxxxx xxxxxxxx x xx xxxxxxxxxx xxxxxxxx xxx xxxxxxx xxxxxx', &
'          xxxxx xxxxxxxxx x xxxxxxxxxx xxxxxx xxxxxxxx xxxxx xxxxxxxx xxxxxxxx', &
'          xxxxx xxx xxxxxxxx xxxxxxx xxxxxxxx xxx xxxx xxx xxxxxxxx xxxxxx', &
'          xxxxxxx xxxxxxx xxxxx xxxxx xx xxxxxx xx xxxxxxxxxx xxxxxx x xxxx', &
'          xxxxxx xxxxxxx x xxx xxxxx xxxxxxxxx xxx xxxxxxx x xxxxxx xxxxxxxxx', &
'          xxxx xxxxxxxxx xxxxxxxx xxxxxxxx xxx xxxxxxx xxxxxxx xxxxxxxxxx', &
'          xxxxxxxxxx xxxxxx xxxxx xxxx xxxxxxx xx xxxxxxxxxx xxxxxx xxxxxx', &
'          xxxxxx xxxx xxxxx', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  ACHAR(3), CHAR(3), IACHAR(3), ICHAR(3), SELECTED_CHAR_KIND(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022            new_line(3fortran)', &
'']

shortname="new_line"
call process()


case('145','nint')

textblock=[character(len=256) :: &
'', &
'nint(3fortran)                                                  nint(3fortran)', &
'', &
'NAME', &
'  NINT(3) - [TYPE:NUMERIC] Nearest whole number', &
'', &
'SYNOPSIS', &
'  result = nint( a [,kind] )', &
'', &
'           elemental integer(kind=KIND) function nint(a, kind )', &
'', &
'            real(kind=**),intent(in) :: a', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  A is type real of any kind', &
'', &
'  o  KIND is a scalar integer constant expression', &
'', &
'  o  The result is default integer kind or the value of KIND if KIND is', &
'     present.', &
'', &
'DESCRIPTION', &
'  NINT(3) rounds its argument to the nearest whole number with its sign', &
'  preserved.', &
'', &
'  The user must ensure the value is a valid value for the range of the KIND', &
'  returned. If the processor cannot represent the result in the kind', &
'  specified, the result is undefined.', &
'', &
'  If A is greater than zero, NINT(A) has the value INT(A+0.5).', &
'', &
'  If A is less than or equal to zero, NINT(A) has the value INT(A-0.5).', &
'', &
'OPTIONS', &
'  o  A : The value to round to the nearest whole number', &
'', &
'  o  KIND : can specify the kind of the output value. If not present, the', &
'     output is the default type of integer.', &
'', &
'RESULT', &
'  The result is the integer nearest A, or if there are two integers equally', &
'  near A, the result is whichever such integer has the greater magnitude.', &
'', &
'  The result is undefined if it cannot be represented in the specified integer', &
'  type.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_nint', &
'      implicit none', &
'      integer,parameter   :: dp=kind(0.0d0)', &
'      real,allocatable    :: in(:)', &
'      integer,allocatable :: out(:)', &
'      integer             :: i', &
'      real                :: x4', &
'      real(kind=dp)       :: x8', &
'', &
'        ! basic use', &
'         x4 = 1.234E0', &
'         x8 = 4.721_dp', &
'         print *, nint(x4), nint(-x4)', &
'         print *, nint(x8), nint(-x8)', &
'', &
'        ! elemental', &
'         in = [ -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, -0.4, &', &
'              &  0.0,   &', &
'              & +0.04, +0.5, +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ]', &
'         out = nint(in)', &
'         do i=1,size(in)', &
'            write(*,*)in(i),out(i)', &
'         enddo', &
'', &
'        ! dusty corners', &
'         ISSUES: block', &
'         use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'         integer :: icheck', &
'            ! make sure input is in range for the type returned', &
'            write(*,*)''Range limits for typical KINDS:''', &
'            write(*,''(1x,g0,1x,g0)'')  &', &
'            & int8,huge(0_int8),   &', &
'            & int16,huge(0_int16), &', &
'            & int32,huge(0_int32), &', &
'            & int64,huge(0_int64)', &
'', &
'            ! the standard does not require this to be an error ...', &
'            x8=12345.67e15 ! too big of a number', &
'            icheck=selected_int_kind(ceiling(log10(x8)))', &
'            write(*,*)''Any KIND big enough? ICHECK='',icheck', &
'            print *, ''These are all wrong answers for '',x8', &
'            print *, nint(x8,kind=int8)', &
'            print *, nint(x8,kind=int16)', &
'            print *, nint(x8,kind=int32)', &
'            print *, nint(x8,kind=int64)', &
'         endblock ISSUES', &
'', &
'      end program demo_nint', &
'', &
'  Results:', &
'', &
'       >               1          -1', &
'       >               5          -5', &
'       >      -2.700000              -3', &
'       >      -2.500000              -3', &
'       >      -2.200000              -2', &
'       >      -2.000000              -2', &
'       >      -1.500000              -2', &
'       >      -1.000000              -1', &
'       >     -0.5000000              -1', &
'       >     -0.4000000               0', &
'       >      0.0000000E+00           0', &
'       >      3.9999999E-02           0', &
'       >      0.5000000               1', &
'       >       1.000000               1', &
'       >       1.500000               2', &
'       >       2.000000               2', &
'       >       2.200000               2', &
'       >       2.500000               3', &
'       >       2.700000               3', &
'       >     Range limits for typical KINDS:', &
'       >     1 127', &
'       >     2 32767', &
'       >     4 2147483647', &
'       >     8 9223372036854775807', &
'       >     Any KIND big enough? ICHECK=          -1', &
'       >     These are all wrong answers for   1.234566949990144E+019', &
'       >        0', &
'       >          0', &
'       >     -2147483648', &
'       >      -9223372036854775808', &
'', &
'STANDARD', &
'  FORTRAN 77 , with KIND argument - Fortran 90', &
'', &
'SEE ALSO', &
'  AINT(3), ANINT(3), INT(3), SELECTED_INT_KIND(3), CEILING(3), FLOOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                nint(3fortran)', &
'']

shortname="nint"
call process()


case('146','norm2')

textblock=[character(len=256) :: &
'', &
'norm2(3fortran)                                                norm2(3fortran)', &
'', &
'NAME', &
'  NORM2(3) - [MATHEMATICS] Euclidean vector norm', &
'', &
'SYNOPSIS', &
'  result = norm2(array, [dim])', &
'', &
'           real(kind=KIND) function norm2(array, dim)', &
'', &
'            real(kind=KIND),intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY shall be an array of type real.', &
'', &
'  o  DIM shall be a scalar of type integer', &
'', &
'  o  The result is of the same type as ARRAY.', &
'', &
'DESCRIPTION', &
'  NORM2(3) calculates the Euclidean vector norm (L_2 norm or generalized L', &
'  norm) of ARRAY along dimension DIM.', &
'', &
'OPTIONS', &
'  o  ARRAY : the array of input values for the L_2 norm computations', &
'', &
'  o  DIM : a value in the range from 1 to RANK(ARRAY).', &
'', &
'RESULT', &
'  If DIM is absent, a scalar with the square root of the sum of squares of the', &
'  elements of ARRAY is returned.', &
'', &
'  Otherwise, an array of rank N-1, where N equals the rank of ARRAY, and a', &
'  shape similar to that of ARRAY with dimension DIM dropped is returned.', &
'', &
'        Case (i):     The result of NORM2 (X) has a value equal to a', &
'                      processor-dependent approximation to the generalized', &
'                      L norm of X, which is the square root of the sum of', &
'                      the squares of the elements of X. If X has size zero,', &
'                      the result has the value zero.', &
'', &
'        Case (ii):    The result of NORM2 (X, DIM=DIM) has a value equal', &
'                      to that of NORM2 (X) if X has rank one. Otherwise,', &
'                      the resulting array is reduced in rank with dimension', &
'                      **dim** removed, and each remaining elment is the', &
'                      result of NORM2(X) for the values along dimension', &
'                      **dim**.', &
'', &
'  It is recommended that the processor compute the result without undue', &
'  overflow or underflow.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_norm2', &
'      implicit none', &
'      integer :: i', &
'      real :: x(2,3) = reshape([ &', &
'         1, 2, 3, &', &
'         4, 5, 6  &', &
'         ],shape(x),order=[2,1])', &
'', &
'        write(*,*) ''input in row-column order''', &
'        write(*,*) ''x=''', &
'        write(*,''(4x,3f4.0)'')transpose(x)', &
'        write(*,*)', &
'        write(*,*) ''norm2(x)='',norm2(x)', &
'        write(*,*) ''which is equivalent to''', &
'        write(*,*) ''sqrt(sum(x**2))='',sqrt(sum(x**2))', &
'        write(*,*)', &
'        write(*,*) ''for reference the array squared is''', &
'        write(*,*) ''x**2=''', &
'        write(*,''(4x,3f4.0)'')transpose(x**2)', &
'        write(*,*)', &
'        write(*,*) ''norm2(x,dim=1)='',norm2(x,dim=1)', &
'        write(*,*) ''norm2(x,dim=2)='',norm2(x,dim=2)', &
'        write(*,*) ''(sqrt(sum(x(:,i)**2)),i=1,3)='',(sqrt(sum(x(:,i)**2)),i=1,3)', &
'        write(*,*) ''(sqrt(sum(x(i,:)**2)),i=1,2)='',(sqrt(sum(x(i,:)**2)),i=1,2)', &
'', &
'      end program demo_norm2', &
'', &
'  Results:', &
'', &
'       >  input in row-column order', &
'       >  x=', &
'       >       1.  2.  3.', &
'       >       4.  5.  6.', &
'       >', &
'       >  norm2(x)=   9.539392', &
'       >  which is equivalent to', &
'       >  sqrt(sum(x**2))=   9.539392', &
'       >', &
'       >  for reference the array squared is', &
'       >  x**2=', &
'       >       1.  4.  9.', &
'       >      16. 25. 36.', &
'       >', &
'       >  norm2(x,dim=1)=   4.123106       5.385165       6.708204', &
'       >  norm2(x,dim=2)=   3.741657       8.774964', &
'       >  (sqrt(sum(x(:,i)**2)),i=1,3)=   4.123106       5.385165       6.708204', &
'       >  (sqrt(sum(x(i,:)**2)),i=1,2)=   3.741657       8.774964', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  PRODUCT(3), SUM(3), HYPOT(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               norm2(3fortran)', &
'']

shortname="norm2"
call process()


case('147','not')

textblock=[character(len=256) :: &
'', &
'not(3fortran)                                                    not(3fortran)', &
'', &
'NAME', &
'  NOT(3) - [BIT:LOGICAL] Logical negation; flips all bits in an integer', &
'', &
'SYNOPSIS', &
'  result = not(i)', &
'', &
'  elemental integer(kind=KIND) function not(i)', &
'', &
'           integer(kind=KIND), intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I may be an integer of any valid kind', &
'', &
'  o  The returned integer is of the same kind as the argument I.', &
'', &
'DESCRIPTION', &
'  NOT(3) returns the bitwise Boolean inverse of I. This is also known as the', &
'  "Bitwise complement" or "Logical negation" of the value.', &
'', &
'  If an input bit is a one, that position is a zero on output. Conversely any', &
'  input bit that is zero is a one on output.', &
'', &
'OPTIONS', &
'  o  I : The value to flip the bits of.', &
'', &
'RESULT', &
'  The result has the value obtained by complementing I bit-by-bit according to', &
'  the following truth table:', &
'', &
'         >    I   |  NOT(I)', &
'         >    ----#----------', &
'         >    1   |   0', &
'         >    0   |   1', &
'', &
'  That is, every input bit is flipped.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'      program demo_not', &
'      implicit none', &
'      integer :: i', &
'        ! basics', &
'         i=-13741', &
'         print *,''the input value'',i,''represented in bits is''', &
'         write(*,''(1x,b32.32,1x,i0)'') i, i', &
'         i=not(i)', &
'         print *,''on output it is'',i', &
'         write(*,''(1x,b32.32,1x,i0)'') i, i', &
'         print *, " on a two''s complement machine flip the bits and add 1"', &
'         print *, " to get the value with the sign changed, for example."', &
'         print *, 1234, not(1234)+1', &
'         print *, -1234, not(-1234)+1', &
'         print *, " of course ''x=-x'' works just fine and more generally."', &
'      end program demo_not', &
'', &
'  Results:', &
'', &
'          the input value      -13741 represented in bits is', &
'          11111111111111111100101001010011 -13741', &
'          on output it is       13740', &
'          00000000000000000011010110101100 13740', &
'           on a two''s complement machine flip the bits and add 1', &
'           to get the value with the sign changed, for example.', &
'                 1234       -1234', &
'                -1234        1234', &
'           of course ''x=-x'' works just fine and more generally.', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  IAND(3), IOR(3), IEOR(3), IBITS(3), IBSET(3),', &
'', &
'  IBCLR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 not(3fortran)', &
'']

shortname="not"
call process()


case('148','null')

textblock=[character(len=256) :: &
'', &
'null(3fortran)                                                  null(3fortran)', &
'', &
'NAME', &
'  NULL(3) - [TRANSFORMATIONAL] Function that returns a disassociated pointer', &
'', &
'SYNOPSIS', &
'  ptr => null( [mold] )', &
'', &
'           function null(mold)', &
'', &
'            type(TYPE(kind=**)),pointer,optional :: mold', &
'', &
'CHARACTERISTICS', &
'  o  MOLD is a pointer of any association status and of any type.', &
'', &
'  o  The result is a disassociated pointer or an unallocated allocatable', &
'     entity.', &
'', &
'DESCRIPTION', &
'  NULL(3) returns a disassociated pointer.', &
'', &
'  If MOLD is present, a disassociated pointer of the same type is returned,', &
'  otherwise the type is determined by context.', &
'', &
'  In Fortran 95, MOLD is optional. Please note that Fortran 2003 includes', &
'  cases where it is required.', &
'', &
'OPTIONS', &
'  o  MOLD : a pointer of any association status and of any type.', &
'', &
'RESULT', &
'  A disassociated pointer or an unallocated allocatable entity.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      !program demo_null', &
'      module showit', &
'      implicit none', &
'      private', &
'      character(len=*),parameter :: g=''(*(g0,1x))''', &
'      public gen', &
'      ! a generic interface that only differs in the', &
'      ! type of the pointer the second argument is', &
'      interface gen', &
'       module procedure s1', &
'       module procedure s2', &
'      end interface', &
'', &
'      contains', &
'', &
'      subroutine s1 (j, pi)', &
'       integer j', &
'       integer, pointer :: pi', &
'         if(associated(pi))then', &
'            write(*,g)''Two integers in S1:,'',j,''and'',pi', &
'         else', &
'            write(*,g)''One integer in S1:,'',j', &
'         endif', &
'      end subroutine s1', &
'', &
'      subroutine s2 (k, pr)', &
'       integer k', &
'       real, pointer :: pr', &
'         if(associated(pr))then', &
'            write(*,g)''integer and real in S2:,'',k,''and'',pr', &
'         else', &
'            write(*,g)''One integer in S2:,'',k', &
'         endif', &
'      end subroutine s2', &
'', &
'      end module showit', &
'', &
'      program demo_null', &
'      use showit, only : gen', &
'', &
'      real,target :: x = 200.0', &
'      integer,target :: i = 100', &
'', &
'      real, pointer :: real_ptr', &
'      integer, pointer :: integer_ptr', &
'', &
'      ! so how do we call S1() or S2() with a disassociated pointer?', &
'', &
'      ! the answer is the null() function with a mold value', &
'', &
'      ! since s1() and s2() both have a first integer', &
'      ! argument the NULL() pointer must be associated', &
'      ! to a real or integer type via the mold option', &
'      ! so the following can distinguish whether s1(1)', &
'      ! or s2() is called, even though the pointers are', &
'      ! not associated or defined', &
'', &
'      call gen (1, null (real_ptr) )    ! invokes s2', &
'      call gen (2, null (integer_ptr) ) ! invokes s1', &
'      real_ptr => x', &
'      integer_ptr => i', &
'      call gen (3, real_ptr ) ! invokes s2', &
'      call gen (4, integer_ptr ) ! invokes s1', &
'', &
'      end program demo_null', &
'', &
'  Results:', &
'', &
'         One integer in S2:, 1', &
'         One integer in S1:, 2', &
'         integer and real in S2:, 3 and 200.000000', &
'         Two integers in S1:, 4 and 100', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  ASSOCIATED(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                null(3fortran)', &
'']

shortname="null"
call process()


case('149','num_images')

textblock=[character(len=256) :: &
'', &
'num_images(3fortran)                                      num_images(3fortran)', &
'', &
'NAME', &
'  NUM_IMAGES(3) - [COLLECTIVE] Number of images', &
'', &
'SYNOPSIS', &
'  result = num_images([team|team_number])', &
'', &
'           integer function num_images (team)', &
'', &
'            type(TEAM_TYPE),intent(in),optional    :: team', &
'            integer(kind=KIND),intent(in),optional :: team_number', &
'', &
'CHARACTERISTICS', &
'  o  use of TEAM and TEAM_NUMBER is mutually exclusive', &
'', &
'  o  TEAM is is a scalar of of type TEAM_TYPE from the intrinsic module', &
'     ISO_FORTRAN_ENV.', &
'', &
'  o  TEAM_NUMBER is an integer scalar.', &
'', &
'  o  the result is a default integer scalar.', &
'', &
'DESCRIPTION', &
'  NUM_IMAGES(3) Returns the number of images.', &
'', &
'OPTIONS', &
'  o  TEAM : shall be a scalar of type TEAM_TYPE from the intrinsic module', &
'     ISO_FORTRAN_ENV, with a value that identifies the current or an ancestor', &
'     team.', &
'', &
'  o  TEAM_NUMBER : identifies the initial team or a team whose parent is the', &
'     same as that of the current team.', &
'', &
'RESULT', &
'  The number of images in the specified team, or in the current team if no', &
'  team is specified.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_num_images', &
'      implicit none', &
'      integer :: value[*]', &
'      real    :: p[*]', &
'      integer :: i', &
'', &
'         value = this_image()', &
'         sync all', &
'         if (this_image() == 1) then', &
'           do i = 1, num_images()', &
'             write(*,''(2(a,i0))'') ''value['', i, ''] is '', value[i]', &
'           end do', &
'         endif', &
'', &
'       ! The following code uses image 1 to read data and', &
'       ! broadcast it to other images.', &
'         if (this_image()==1) then', &
'            p=1234.5678', &
'            do i = 2, num_images()', &
'               p[i] = p', &
'            end do', &
'         end if', &
'         sync all', &
'', &
'      end program demo_num_images', &
'', &
'STANDARD', &
'  Fortran 2008 . With DISTANCE or FAILED argument, TS 18508', &
'', &
'SEE ALSO', &
'  THIS_IMAGE(3), IMAGE_INDEX(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022          num_images(3fortran)', &
'']

shortname="num_images"
call process()


case('150','out_of_range')

textblock=[character(len=256) :: &
'', &
'out_of_range(3fortran)                                  out_of_range(3fortran)', &
'', &
'NAME', &
'  OUT_OF_RANGE(3) - [TYPE:NUMERIC] Whether a value cannot be converted safely.', &
'', &
'SYNOPSIS', &
'  result = out_of_range (x, mold [, round])', &
'', &
'           elemental logical function(x, mold, round)', &
'', &
'            TYPE,kind=KIND),intent(in) :: x', &
'            TYPE,kind=KIND),intent(in) :: mold', &
'            logical,intent(in),optional     :: round', &
'', &
'CHARACTERISTICS', &
'  o  X is of type integer or real.', &
'', &
'  o  MOLD is an integer or real scalar.', &
'', &
'  o  ROUND is a logical scalar.', &
'', &
'  o  the result is a default logical.', &
'', &
'DESCRIPTION', &
'  OUT_OF_RANGE(3) determines whether a value X can be converted safely to a', &
'  real or integer variable the same type and kind as MOLD.', &
'', &
'  For example, if INT8 is the kind value for an 8-bit binary integer type,', &
'  OUT_OF_RANGE(-128.5, 0_INT8) will have the value false and', &
'  OUT_OF_RANGE(-128.5, 0_INT8, .TRUE.) will have the value .true. because the', &
'  value will be truncated when converted to an integer and -128 is a', &
'  representable value on a two''s complement machine in eight bits even though', &
'  +128 is not.', &
'', &
'OPTIONS', &
'  o  X : a scalar to be tested for whether it can be stored in a variable of', &
'     the type and kind of MOLD', &
'', &
'  o  MOLD and kind are queried to determine the characteristics of what needs', &
'     to be fit into.', &
'', &
'  o  ROUND : flag whether to round the value of XX before validating it as an', &
'     integer value like MOLD.', &
'', &
'     ROUND can only be present if X is of type real and MOLD is of type', &
'     integer.', &
'', &
'RESULT', &
'  From the standard:', &
'', &
'  Case (i): If MOLD is of type integer, and ROUND is absent or present with', &
'  the value false, the result is true if and only if the value of X is an IEEE', &
'  infinity or NaN, or if the integer with largest magnitude that lies between', &
'  zero and X inclusive is not representable by objects with the type and kind', &
'  of MOLD.', &
'', &
'  Case (ii): If MOLD is of type integer, and ROUND is present with the value', &
'  true, the result is true if and only if the value of X is an IEEE infinity', &
'  or NaN, or if the integer nearest X, or the integer of greater magnitude if', &
'  two integers are equally near to X, is not representable by objects with the', &
'  type and kind of MOLD.', &
'', &
'  Case (iii): Otherwise, the result is true if and only if the value of X is', &
'  an IEEE infinity or NaN that is not supported by objects of the type and', &
'  kind of MOLD, or if X is a finite number and the result of rounding the', &
'  value of X (according to the IEEE rounding mode if appropriate) to the', &
'  extended model for the kind of MOLD has magnitude larger than that of the', &
'  largest finite number with the same sign as X that is representable by', &
'  objects with the type and kind of MOLD.', &
'', &
'NOTE', &
'  MOLD is required to be a scalar because the only information taken from it', &
'  is its type and kind. Allowing an array MOLD would require that it be', &
'  conformable with X. ROUND is scalar because allowing an array rounding mode', &
'  would have severe performance difficulties on many processors.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_out_of_range', &
'      use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      use, intrinsic :: iso_fortran_env, only : real32, real64, real128', &
'      implicit none', &
'      integer            :: i', &
'      integer(kind=int8) :: i8, j8', &
'', &
'          ! compilers are not required to produce an error on out of range.', &
'          ! here storing the default integers into 1-byte integers', &
'          ! incorrectly can have unexpected results', &
'          do i=127,130', &
'             i8=i', &
'             j8=-i', &
'             ! OUT_OF_RANGE(3f) can let you check if the value will fit', &
'             write(*,*)i8,j8,'' might have expected'',i,-i, &', &
'              & out_of_range( i,i8), &', &
'              & out_of_range(-i,i8)', &
'          enddo', &
'          write(*,*) ''RANGE IS '',-1-huge(0_int8),''TO'',huge(0_int8)', &
'          ! the real -128.5 is truncated to -128 and is in range', &
'          write(*,*) out_of_range (  -128.5, 0_int8)         ! false', &
'', &
'          ! the real -128.5 is rounded to -129 and is not in range', &
'          write(*,*) out_of_range (  -128.5, 0_int8, .true.) ! true', &
'', &
'      end program demo_out_of_range', &
'', &
'  Results:', &
'', &
'        >  127 -127  might have expected         127        -127 F F', &
'        > -128 -128  might have expected         128        -128 T F', &
'        > -127  127  might have expected         129        -129 T T', &
'        > -126  126  might have expected         130        -130 T T', &
'        > RANGE IS         -128 TO  127', &
'        > F', &
'        > T', &
'', &
'STANDARD', &
'  FORTRAN 2018', &
'', &
'SEE ALSO', &
'  o  AIMAG(3) - Imaginary part of complex number', &
'', &
'  o  CMPLX(3) - Convert values to a complex type', &
'', &
'  o  DBLE(3) - Double conversion function', &
'', &
'  o  INT(3) - Truncate towards zero and convert to integer', &
'', &
'  o  NINT(3) - Nearest whole number', &
'', &
'  o  REAL(3) - Convert to real type', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022        out_of_range(3fortran)', &
'']

shortname="out_of_range"
call process()


case('151','pack')

textblock=[character(len=256) :: &
'', &
'pack(3fortran)                                                  pack(3fortran)', &
'', &
'NAME', &
'  PACK(3) - [ARRAY:CONSTRUCTION] Pack an array into an array of rank one', &
'', &
'SYNOPSIS', &
'  result = pack( array, mask [,vector] )', &
'', &
'           TYPE(kind=KIND) function pack(array,mask,vector)', &
'', &
'            TYPE(kind=KIND),option(in) :: array(..)', &
'            logical  :: mask(..)', &
'            TYPE(kind=KIND),option(in),optional :: vector(*)', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY is an array of any type', &
'', &
'  o  MASK a logical scalar as well as an array conformable with ARRAY.', &
'', &
'  o  VECTOR is of the same kind and type as ARRAY and of rank one', &
'', &
'  o  the returned value is of the same kind and type as ARRAY', &
'', &
'DESCRIPTION', &
'  PACK(3) stores the elements of ARRAY in an array of rank one.', &
'', &
'  The beginning of the resulting array is made up of elements whose MASK', &
'  equals .true.. Afterwards, remaining positions are filled with elements', &
'  taken from VECTOR', &
'', &
'OPTIONS', &
'  o  ARRAY : The data from this array is used to fill the resulting vector', &
'', &
'  o  MASK : the logical mask must be the same size as ARRAY or, alternatively,', &
'     it may be a logical scalar.', &
'', &
'  o  VECTOR : an array of the same type as ARRAY and of rank one. If present,', &
'     the number of elements in VECTOR shall be equal to or greater than the', &
'     number of true elements in MASK. If MASK is scalar, the number of', &
'     elements in VECTOR shall be equal to or greater than the number of', &
'     elements in ARRAY.', &
'', &
'  VECTOR shall have at least as many elements as there are in ARRAY.', &
'', &
'RESULT', &
'  The result is an array of rank one and the same type as that of ARRAY.  If', &
'  VECTOR is present, the result size is that of VECTOR, the number of .true.', &
'  values in MASK otherwise.', &
'', &
'  If MASK is scalar with the value .true., in which case the result size is', &
'  the size of ARRAY.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_pack', &
'      implicit none', &
'      integer, allocatable :: m(:)', &
'      character(len=10) :: c(4)', &
'', &
'       ! gathering nonzero elements from an array:', &
'         m = [ 1, 0, 0, 0, 5, 0 ]', &
'         write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0)', &
'', &
'       ! Gathering nonzero elements from an array and appending elements', &
'       ! from VECTOR till the size of the mask array (or array size if the', &
'       ! mask is scalar):', &
'         m = [ 1, 0, 0, 2 ]', &
'         write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0, [ 0, 0, 3, 4 ])', &
'         write(*, fmt="(*(i0, '' ''))") pack(m, m /= 0 )', &
'', &
'       ! select strings whose second character is "a"', &
'         c = [ character(len=10) :: ''ape'', ''bat'', ''cat'', ''dog'']', &
'         write(*, fmt="(*(g0, '' ''))") pack(c, c(:)(2:2) == ''a'' )', &
'', &
'      end program demo_pack', &
'', &
'  Results:', &
'', &
'       > 1 5', &
'       > 1 2 3 4', &
'       > 1 2', &
'       > bat        cat', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  MERGE(3), SPREAD(3), UNPACK(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                pack(3fortran)', &
'']

shortname="pack"
call process()


case('152','parity')

textblock=[character(len=256) :: &
'', &
'parity(3fortran)                                              parity(3fortran)', &
'', &
'NAME', &
'  PARITY(3) - [ARRAY:REDUCTION] Array reduction by .NEQV. operation', &
'', &
'SYNOPSIS', &
'  result = parity( mask [,dim] )', &
'', &
'           logical(kind=KIND) function parity(mask, dim)', &
'', &
'            type(logical(kind=KIND)),intent(in)        :: mask(..)', &
'            type(integer(kind=**)),intent(in),optional :: dim', &
'', &
'CHARACTERISTICS', &
'  o  MASK is a logical array', &
'', &
'  o  DIM is an integer scalar', &
'', &
'  o  the result is of type logical with the same kind type parameter as MASK.', &
'     It is a scalar if DIM does not appear; otherwise it is the rank and shape', &
'     of MASK with the dimension specified by DIM removed.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  PARITY(3) calculates the parity array (i.e. the reduction using .neqv.)  of', &
'  MASK along dimension DIM if DIM is present and not 1. Otherwise, it returns', &
'  the parity of the entire MASK array as a scalar.', &
'', &
'OPTIONS', &
'  o  MASK : Shall be an array of type logical.', &
'', &
'  o  DIM : (Optional) shall be a scalar of type integer with a value in the', &
'     range from 1 to n, where n equals the rank of MASK.', &
'', &
'RESULT', &
'  The result is of the same type as MASK.', &
'', &
'  If DIM is absent, a scalar with the parity of all elements in MASK is', &
'  returned: .true. if an odd number of elements are .true. and .false.', &
'  otherwise.', &
'', &
'  If MASK has rank one, PARITY (MASK, DIM) is equal to PARITY (MASK).', &
'  Otherwise, the result is an array of parity values with dimension DIM', &
'  dropped.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_parity', &
'      implicit none', &
'      logical, parameter :: T=.true., F=.false.', &
'      logical :: x(3,4)', &
'        ! basics', &
'         print *, parity([T,F])', &
'         print *, parity([T,F,F])', &
'         print *, parity([T,F,F,T])', &
'         print *, parity([T,F,F,T,T])', &
'         x(1,:)=[T,T,T,T]', &
'         x(2,:)=[T,T,T,T]', &
'         x(3,:)=[T,T,T,T]', &
'         print *, parity(x)', &
'         print *, parity(x,dim=1)', &
'         print *, parity(x,dim=2)', &
'      end program demo_parity', &
'', &
'  Results:', &
'', &
'       >  T', &
'       >  T', &
'       >  F', &
'       >  T', &
'       >  F', &
'       >  T T T T', &
'       >  F F F', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  o  ALL(3) - Determines if all the values are true', &
'', &
'  o  ANY(3) - Determines if any of the values in the logical array are .true.', &
'', &
'  o  COUNT(3) - Count true values in an array', &
'', &
'  o  SUM(3) - Sum the elements of an array', &
'', &
'  o  MAXVAL(3) - Determines the maximum value in an array or row', &
'', &
'  o  MINVAL(3) - Minimum value of an array', &
'', &
'  o  PRODUCT(3) - Product of array elements', &
'', &
'  o  REDUCE(3) - General array reduction', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              parity(3fortran)', &
'']

shortname="parity"
call process()


case('153','popcnt')

textblock=[character(len=256) :: &
'', &
'popcnt(3fortran)                                              popcnt(3fortran)', &
'', &
'NAME', &
'  POPCNT(3) - [BIT:COUNT] Number of bits set', &
'', &
'SYNOPSIS', &
'  result = popcnt(i)', &
'', &
'           elemental integer function popcnt(i)', &
'', &
'            integer(kind=KIND), intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I may be an integer of any kind.', &
'', &
'  o  The return value is an integer of the default integer kind.', &
'', &
'DESCRIPTION', &
'  POPCNT(3) returns the number of bits set to one in the binary representation', &
'  of an integer.', &
'', &
'OPTIONS', &
'  o  I : value to count set bits in', &
'', &
'RESULT', &
'  The number of bits set to one in I.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_popcnt', &
'      use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'         & int8, int16, int32, int64', &
'      implicit none', &
'      character(len=*),parameter :: pretty=''(b64,1x,i0)''', &
'        ! basic usage', &
'         print pretty, 127,     popcnt(127)', &
'         print pretty, int(b"01010"), popcnt(int(b"01010"))', &
'', &
'        ! any kind of an integer can be used', &
'         print pretty, huge(0_int8),  popcnt(huge(0_int8))', &
'         print pretty, huge(0_int16), popcnt(huge(0_int16))', &
'         print pretty, huge(0_int32), popcnt(huge(0_int32))', &
'         print pretty, huge(0_int64), popcnt(huge(0_int64))', &
'      end program demo_popcnt', &
'', &
'  Results:', &
'', &
'  Note that on most machines the first bit is the sign bit, and a zero is used', &
'  for positive values; but that this is system-dependent. These are typical', &
'  values, where the huge(3f) function has set all but the first bit to 1.', &
'', &
'       >                                                         1111111 7', &
'       >                                                            1010 2', &
'       >                                                         1111111 7', &
'       >                                                 111111111111111 15', &
'       >                                 1111111111111111111111111111111 31', &
'       > 111111111111111111111111111111111111111111111111111111111111111 63', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  There are many procedures that operator or query values at the bit level:', &
'', &
'  POPPAR(3), LEADZ(3), TRAILZ(3) ATOMIC_AND(3), ATOMIC_FETCH_AND(3),', &
'  ATOMIC_FETCH_OR(3), ATOMIC_FETCH_XOR(3), ATOMIC_OR(3), ATOMIC_XOR(3),', &
'  BGE(3), BGT(3), BIT_SIZE(3), BLE(3), BLT(3), BTEST(3), DSHIFTL(3),', &
'  DSHIFTR(3), IALL(3), IAND(3), IANY(3), IBCLR(3), IBITS(3), IBSET(3),', &
'  IEOR(3), IOR(3), IPARITY(3), ISHFTC(3), ISHFT(3), MASKL(3), MASKR(3),', &
'  MERGE_BITS(3), MVBITS(3), NOT(3), SHIFTA(3), SHIFTL(3), SHIFTR(3),', &
'  STORAGE_SIZE(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              popcnt(3fortran)', &
'']

shortname="popcnt"
call process()


case('154','poppar')

textblock=[character(len=256) :: &
'', &
'poppar(3fortran)                                              poppar(3fortran)', &
'', &
'NAME', &
'  POPPAR(3) - [BIT:COUNT] Parity of the number of bits set', &
'', &
'SYNOPSIS', &
'  result = poppar(i)', &
'', &
'           elemental integer function poppar(i)', &
'', &
'            integer(kind=KIND), intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I is an integer of any kind', &
'', &
'  o  the return value is a default kind integer', &
'', &
'DESCRIPTION', &
'  POPPAR(3) returns the parity of an integer''s binary representation (i.e.,', &
'  the parity of the number of bits set).', &
'', &
'  The parity is expressed as', &
'', &
'  o  0 (zero) if I has an even number of bits set to 1.', &
'', &
'  o  1 (one) if the number of bits set to one 1 is odd,', &
'', &
'OPTIONS', &
'  o  I : The value to query for its bit parity', &
'', &
'RESULT', &
'  The return value is equal to 0 if I has an even number of bits set and 1 if', &
'  an odd number of bits are set.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_poppar', &
'      use, intrinsic :: iso_fortran_env, only : integer_kinds, &', &
'         & int8, int16, int32, int64', &
'      implicit none', &
'      character(len=*),parameter :: pretty=''(b64,1x,i0)''', &
'         ! basic usage', &
'         print pretty, 127,     poppar(127)', &
'         print pretty, 128,     poppar(128)', &
'         print pretty, int(b"01010"), poppar(int(b"01010"))', &
'', &
'         ! any kind of an integer can be used', &
'         print pretty, huge(0_int8),  poppar(huge(0_int8))', &
'         print pretty, huge(0_int16), poppar(huge(0_int16))', &
'         print pretty, huge(0_int32), poppar(huge(0_int32))', &
'         print pretty, huge(0_int64), poppar(huge(0_int64))', &
'      end program demo_poppar', &
'', &
'  Results:', &
'', &
'       >                                                          1111111 1', &
'       >                                                         10000000 1', &
'       >                                                             1010 0', &
'       >                                  1111111111111111111111111111111 1', &
'       >                                                          1111111 1', &
'       >                                                  111111111111111 1', &
'       >                                  1111111111111111111111111111111 1', &
'       >  111111111111111111111111111111111111111111111111111111111111111 1', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  There are many procedures that operator or query values at the bit level:', &
'', &
'  POPCNT(3), LEADZ(3), TRAILZ(3) ATOMIC_AND(3), ATOMIC_FETCH_AND(3),', &
'  ATOMIC_FETCH_OR(3), ATOMIC_FETCH_XOR(3), ATOMIC_OR(3), ATOMIC_XOR(3),', &
'  BGE(3), BGT(3), BIT_SIZE(3), BLE(3), BLT(3), BTEST(3), DSHIFTL(3),', &
'  DSHIFTR(3), IALL(3), IAND(3), IANY(3), IBCLR(3), IBITS(3), IBSET(3),', &
'  IEOR(3), IOR(3), IPARITY(3), ISHFTC(3), ISHFT(3), MASKL(3), MASKR(3),', &
'  MERGE_BITS(3), MVBITS(3), NOT(3), SHIFTA(3), SHIFTL(3), SHIFTR(3),', &
'  STORAGE_SIZE(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              poppar(3fortran)', &
'']

shortname="poppar"
call process()


case('155','precision')

textblock=[character(len=256) :: &
'', &
'precision(3fortran)                                        precision(3fortran)', &
'', &
'NAME', &
'  PRECISION(3) - [NUMERIC MODEL] Decimal precision of a real kind', &
'', &
'SYNOPSIS', &
'  result = precision(x)', &
'', &
'           integer function precision(x)', &
'', &
'            TYPE(kind=**),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X shall be of type real or complex. It may be a scalar or an array.', &
'', &
'  o  the result is a default integer scalar.', &
'', &
'DESCRIPTION', &
'  PRECISION(3) returns the decimal precision in the model of the type of X.', &
'', &
'OPTIONS', &
'  o  X : the type and kind of the argument are used to determine which number', &
'     model to query. The value of the argument is not unused; it may even be', &
'     undefined.', &
'', &
'RESULT', &
'  The precision of values of the type and kind of X', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_precision', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'      implicit none', &
'      real(kind=sp)    :: x(2)', &
'      complex(kind=dp) :: y', &
'', &
'         print *, precision(x), range(x)', &
'         print *, precision(y), range(y)', &
'', &
'      end program demo_precision', &
'', &
'  Results:', &
'', &
'        >           6          37', &
'        >          15         307', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), RADIX(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022           precision(3fortran)', &
'']

shortname="precision"
call process()


case('156','present')

textblock=[character(len=256) :: &
'', &
'present(3fortran)                                            present(3fortran)', &
'', &
'NAME', &
'  PRESENT(3) - [STATE:INQUIRY] Determine whether an optional dummy argument is', &
'  specified', &
'', &
'SYNOPSIS', &
'  result = present(a)', &
'', &
'           logical function present (a)', &
'', &
'            type(TYPE(kind=KIND)) :: a(..)', &
'', &
'CHARACTERISTICS', &
'  o  A May be of any type and may be a pointer, scalar or array value, or a', &
'     dummy procedure.', &
'', &
'DESCRIPTION', &
'  PRESENT(3) can be used in a procedure to determine if an optional dummy', &
'  argument was present on the current call to the procedure.', &
'', &
'  A shall be the name of an optional dummy argument that is accessible in the', &
'  subprogram in which the PRESENT(3) function reference appears. There are no', &
'  other requirements on A.', &
'', &
'  Note when an argument is not present when the current procedure is invoked,', &
'  you may only pass it as an optional argument to another procedure or pass it', &
'  as an argument to PRESENT.', &
'', &
'OPTIONS', &
'  o  A : the name of an optional dummy argument accessible within the current', &
'     subroutine or function.', &
'', &
'RESULT', &
'  Returns .true. if the optional argument A is present (was passed on the call', &
'  to the procedure) , or .false. otherwise.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_present', &
'      implicit none', &
'      integer :: answer', &
'         ! argument to func() is not present', &
'         answer=func()', &
'         write(*,*) answer', &
'         ! argument to func() is present', &
'         answer=func(1492)', &
'         write(*,*) answer', &
'      contains', &
'', &
'      integer function func(x)', &
'      ! the optional characteristic on this definition allows this variable', &
'      ! to not be specified on a call; and also allows it to subsequently', &
'      ! be passed to PRESENT(3):', &
'      integer, intent(in), optional :: x', &
'      integer :: x_local', &
'', &
'        ! basic', &
'         if(present(x))then', &
'           ! if present, you can use x like any other variable.', &
'           x_local=x', &
'         else', &
'           ! if not, you cannot define or reference x except to', &
'           ! pass it as an optional parameter to another procedure', &
'           ! or in a call to present(3f)', &
'           x_local=0', &
'         endif', &
'', &
'         func=x_local**2', &
'', &
'        ! passing the argument on to other procedures', &
'         ! so something like this is a bad idea because x is used', &
'         ! as the first argument to merge(3f) when it might not be', &
'         ! present', &
'         ! xlocal=merge(x,0,present(x)) ! NO!!', &
'', &
'         ! We can pass it to another procedure if another', &
'         ! procedure declares the argument as optional as well,', &
'         ! or we have tested that X is present', &
'         call tattle(''optional argument x'',x)', &
'         if(present(x))call not_optional(x)', &
'      end function', &
'', &
'      subroutine tattle(label,arg)', &
'      character(len=*),intent(in) :: label', &
'      integer,intent(in),optional :: arg', &
'         if(present(arg))then', &
'            write(*,*)label,'' is present''', &
'         else', &
'            write(*,*)label,'' is not present''', &
'         endif', &
'      end subroutine tattle', &
'', &
'      subroutine not_optional(arg)', &
'      integer,intent(in) :: arg', &
'         write(*,*)''already tested X is defined'',arg', &
'      end subroutine not_optional', &
'', &
'      end program demo_present', &
'', &
'  Results:', &
'', &
'          optional argument x is not present', &
'                    0', &
'          optional argument x is present', &
'          already tested X is defined 1492', &
'              2226064', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             present(3fortran)', &
'']

shortname="present"
call process()


case('157','product')

textblock=[character(len=256) :: &
'', &
'product(3fortran)                                            product(3fortran)', &
'', &
'NAME', &
'  PRODUCT(3) - [ARRAY:REDUCTION] Product of array elements', &
'', &
'SYNOPSIS', &
'  result = product(array [,dim] [,mask])', &
'', &
'           NUMERIC function product(array, dim, mask)', &
'', &
'            NUMERIC,intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  NUMERIC is any numeric type and kind.', &
'', &
'DESCRIPTION', &
'  PRODUCT(3) multiplies together all the selected elements of ARRAY, or along', &
'  dimension DIM if the corresponding element in MASK is .true..', &
'', &
'  If DIM is absent, a scalar with the product of all elements in ARRAY is', &
'  returned. (Note a zero-sized ARRAY returns 1).', &
'', &
'  When DIM is present, If the masked array has a dimension of one (ie. is a', &
'  vector) the result is a scalar. Otherwise, an array of rank N-1, where N', &
'  equals the rank of ARRAY, and a shape similar to that of ARRAY with', &
'  dimension DIM dropped is returned.', &
'', &
'OPTIONS', &
'  o  ARRAY : Shall be an array of type integer, real or complex.', &
'', &
'  o  DIM : shall be a scalar of type integer with a value in the range from 1', &
'     TO N, where N equals the rank of ARRAY.', &
'', &
'  o  MASK : shall be of type logical and either be a scalar or an array of the', &
'     same shape as ARRAY.', &
'', &
'RESULT', &
'  The result is of the same type as ARRAY.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_product', &
'      implicit none', &
'      character(len=*),parameter :: all=''(*(g0,1x))'' ! a handy format', &
'      character(len=1),parameter :: nl=new_line(''a'')', &
'', &
'      NO_DIM: block', &
'      !    If DIM is not specified, the result is the product of all the', &
'      !    selected array elements.', &
'      integer :: i,n, p1, p2', &
'      integer,allocatable :: array(:)', &
'         ! all elements are selected by default', &
'         do n=1,10', &
'            print all, ''factorial of '',n,'' is '', product([(real(i),i=1,n)])', &
'         enddo', &
'', &
'         ! using a mask', &
'         array=[10,12,13,15,20,25,30]', &
'         p1=product(array, mask=mod(array, 2)==1) ! only odd elements', &
'         p2=product(array, mask=mod(array, 2)/=1) ! only even elements', &
'         print all, nl,''product of all elements'',product(array) ! all elements', &
'         print all, '' odd * even ='',nl,p1,''*'',p2,''='',p1*p2', &
'', &
'         ! NOTE: If ARRAY is a zero-sized array, the result is equal to one', &
'         print all', &
'         print all, ''zero-sized array=>'',product([integer :: ])', &
'         ! NOTE: If nothing in the mask is true, this also results in a null', &
'         !       array', &
'         print all, ''all elements have a false mask=>'', &', &
'                  & product(array,mask=.false.)', &
'', &
'      endblock NO_DIM', &
'', &
'      WITH_DIM: block', &
'      integer :: rect(2,3)', &
'      integer :: box(2,3,4)', &
'', &
'      !  lets fill a few arrays', &
'         rect = reshape([ &', &
'           1, 2, 3,       &', &
'           4, 5, 6        &', &
'         ],shape(rect),order=[2,1])', &
'         call print_matrix_int(''rect'',rect)', &
'', &
'      !  Find the product of each column in RECT.', &
'         print all, ''product of columns='',product(rect, dim = 1)', &
'', &
'      ! Find the product of each row in RECT.', &
'         print all, ''product of rows='',product(rect, dim = 2)', &
'', &
'      ! now lets try a box', &
'         box(:,:,1)=rect', &
'         box(:,:,2)=rect*(+10)', &
'         box(:,:,3)=rect*(-10)', &
'         box(:,:,4)=rect*2', &
'         ! lets look at the values', &
'         call print_matrix_int(''box 1'',box(:,:,1))', &
'         call print_matrix_int(''box 2'',box(:,:,2))', &
'         call print_matrix_int(''box 3'',box(:,:,3))', &
'         call print_matrix_int(''box 4'',box(:,:,4))', &
'', &
'         ! remember without dim= even a box produces a scalar', &
'         print all, ''no dim gives a scalar'',product(real(box))', &
'', &
'         ! only one plane has negative values, so note all the "1" values', &
'         ! for vectors with no elements', &
'         call print_matrix_int(''negative values'', &', &
'         & product(box,mask=box < 0,dim=1))', &
'', &
'      !   If DIM is specified and ARRAY has rank greater than one, the', &
'      !   result is a new array in which dimension DIM has been eliminated.', &
'', &
'         ! pick a dimension to multiply though', &
'         call print_matrix_int(''dim=1'',product(box,dim=1))', &
'', &
'         call print_matrix_int(''dim=2'',product(box,dim=2))', &
'', &
'         call print_matrix_int(''dim=3'',product(box,dim=3))', &
'', &
'      endblock WITH_DIM', &
'', &
'      contains', &
'', &
'         subroutine print_matrix_int(title,arr)', &
'         implicit none', &
'', &
'         !@(#) print small 2d integer arrays in row-column format', &
'', &
'         character(len=*),intent(in)  :: title', &
'         integer,intent(in)           :: arr(:,:)', &
'         integer                      :: i', &
'         character(len=:),allocatable :: biggest', &
'', &
'            print all', &
'            print all, trim(title),'':('',shape(arr),'')''  ! print title', &
'            biggest=''           ''  ! make buffer to write integer into', &
'            ! find how many characters to use for integers', &
'            write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'            ! use this format to write a row', &
'            biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'            ! print one row of array at a time', &
'            do i=1,size(arr,dim=1)', &
'               write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'', &
'         end subroutine print_matrix_int', &
'', &
'      end program demo_product', &
'', &
'  Results:', &
'', &
'      factorial of  1  is  1.000000', &
'      factorial of  2  is  2.000000', &
'      factorial of  3  is  6.000000', &
'      factorial of  4  is  24.00000', &
'      factorial of  5  is  120.0000', &
'      factorial of  6  is  720.0000', &
'      factorial of  7  is  5040.000', &
'      factorial of  8  is  40320.00', &
'      factorial of  9  is  362880.0', &
'      factorial of  10  is  3628800.', &
'', &
'       product of all elements 351000000', &
'       odd * even =', &
'       4875 * 72000 = 351000000', &
'', &
'      zero-sized array=> 1', &
'      all elements have a false mask=> 1', &
'', &
'      rect :( 2 3 )', &
'       > [  1,  2,  3 ]', &
'       > [  4,  5,  6 ]', &
'      product of columns= 4 10 18', &
'      product of rows= 6 120', &
'', &
'      box 1 :( 2 3 )', &
'       > [  1,  2,  3 ]', &
'       > [  4,  5,  6 ]', &
'', &
'      box 2 :( 2 3 )', &
'       > [  10,  20,  30 ]', &
'       > [  40,  50,  60 ]', &
'', &
'      box 3 :( 2 3 )', &
'       > [ -10, -20, -30 ]', &
'       > [ -40, -50, -60 ]', &
'', &
'      box 4 :( 2 3 )', &
'       > [   2,   4,   6 ]', &
'       > [   8,  10,  12 ]', &
'      no dim gives a scalar .1719927E+26', &
'', &
'      negative values :( 3 4 )', &
'       > [     1,     1,   400,     1 ]', &
'       > [     1,     1,  1000,     1 ]', &
'       > [     1,     1,  1800,     1 ]', &
'', &
'      dim=1 :( 3 4 )', &
'       > [     4,   400,   400,    16 ]', &
'       > [    10,  1000,  1000,    40 ]', &
'       > [    18,  1800,  1800,    72 ]', &
'', &
'      dim=2 :( 2 4 )', &
'       > [       6,    6000,   -6000,      48 ]', &
'       > [     120,  120000, -120000,     960 ]', &
'', &
'      dim=3 :( 2 3 )', &
'       > [    -200,   -3200,  -16200 ]', &
'       > [  -51200, -125000, -259200 ]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  SUM(3), note that an element by element multiplication is done directly', &
'  using the star character.', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             product(3fortran)', &
'']

shortname="product"
call process()


case('158','radix')

textblock=[character(len=256) :: &
'', &
'radix(3fortran)                                                radix(3fortran)', &
'', &
'NAME', &
'  RADIX(3) - [NUMERIC MODEL] Base of a numeric model', &
'', &
'SYNOPSIS', &
'  result = radix(x)', &
'', &
'          integer function radix(x)', &
'', &
'           TYPE(kind=**),intent(in) :: x(..)', &
'', &
'CHARACTERISTICS', &
'  o  X may be scalar or an array of any real or integer type.', &
'', &
'  o  the result is a default integer scalar.', &
'', &
'DESCRIPTION', &
'  RADIX(3) returns the base of the internal model representing the numeric', &
'  entity X.', &
'', &
'  In a positional numeral system, the radix or base is the number of unique', &
'  digits, including the digit zero, used to represent numbers.', &
'', &
'  This function helps to represent the internal computing model generically,', &
'  but will be 2 (representing a binary machine) for any common platform for', &
'  all the numeric types.', &
'', &
'OPTIONS', &
'  o  X : used to identify the type of number to query.', &
'', &
'RESULT', &
'  The returned value indicates what base is internally used to represent the', &
'  type of numeric value X represents.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_radix', &
'      implicit none', &
'         print *, "The radix for the default integer kind is", radix(0)', &
'         print *, "The radix for the default real kind is", radix(0.0)', &
'         print *, "The radix for the doubleprecision real kind is", radix(0.0d0)', &
'      end program demo_radix', &
'', &
'  Results:', &
'', &
'       >  The radix for the default integer kind is           2', &
'       >  The radix for the default real kind is           2', &
'       >  The radix for the doubleprecision real kind is           2', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RANGE(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               radix(3fortran)', &
'']

shortname="radix"
call process()


case('159','random_number')

textblock=[character(len=256) :: &
'', &
'random_number(3fortran)                                random_number(3fortran)', &
'', &
'NAME', &
'  RANDOM_NUMBER(3) - [MATHEMATICS:RANDOM] Pseudo-random number', &
'', &
'SYNOPSIS', &
'  call random_number(harvest)', &
'', &
'           subroutine random_number(harvest)', &
'', &
'            real,intent(out) :: harvest(..)', &
'', &
'CHARACTERISTICS', &
'  o  HARVEST and the result are default real variables', &
'', &
'DESCRIPTION', &
'  RANDOM_NUMBER(3) returns a single pseudorandom number or an array of', &
'  pseudorandom numbers from the uniform distribution over the range 0 <= x <', &
'  1.', &
'', &
'OPTIONS', &
'  o  HARVEST : Shall be a scalar or an array of type real.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_random_number', &
'      use, intrinsic :: iso_fortran_env, only : dp=>real64', &
'      implicit none', &
'      integer, allocatable :: seed(:)', &
'      integer              :: n', &
'      integer              :: first,last', &
'      integer              :: i', &
'      integer              :: rand_int', &
'      integer,allocatable  :: count(:)', &
'      real(kind=dp)        :: rand_val', &
'         call random_seed(size = n)', &
'         allocate(seed(n))', &
'         call random_seed(get=seed)', &
'         first=1', &
'         last=10', &
'         allocate(count(last-first+1))', &
'         ! To have a discrete uniform distribution on the integers', &
'         ! [first, first+1, ..., last-1, last] carve the continuous', &
'         ! distribution up into last+1-first equal sized chunks,', &
'         ! mapping each chunk to an integer.', &
'         !', &
'         ! One way is:', &
'         !   call random_number(rand_val)', &
'         ! choose one from last-first+1 integers', &
'         !   rand_int = first + FLOOR((last+1-first)*rand_val)', &
'            count=0', &
'            ! generate a lot of random integers from 1 to 10 and count them.', &
'            ! with a large number of values you should get about the same', &
'            ! number of each value', &
'            do i=1,100000000', &
'               call random_number(rand_val)', &
'               rand_int=first+floor((last+1-first)*rand_val)', &
'               if(rand_int.ge.first.and.rand_int.le.last)then', &
'                  count(rand_int)=count(rand_int)+1', &
'               else', &
'                  write(*,*)rand_int,'' is out of range''', &
'               endif', &
'            enddo', &
'            write(*,''(i0,1x,i0)'')(i,count(i),i=1,size(count))', &
'      end program demo_random_number', &
'', &
'  Results:', &
'', &
'    1 10003588 2 10000104 3 10000169 4 9997996 5 9995349 6 10001304 7 10001909', &
'    8 9999133 9 10000252 10 10000196', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  RANDOM_SEED(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022       random_number(3fortran)', &
'']

shortname="random_number"
call process()


case('160','random_seed')

textblock=[character(len=256) :: &
'', &
'random_seed(3fortran)                                    random_seed(3fortran)', &
'', &
'NAME', &
'  RANDOM_SEED(3) - [MATHEMATICS:RANDOM] Initialize a pseudo-random number', &
'  sequence', &
'', &
'SYNOPSIS', &
'  call random_seed( [size] [,put] [,get] )', &
'', &
'           subroutine random_seed( size, put, get )', &
'', &
'            integer,intent(out),optional :: size', &
'            integer,intent(in),optional :: put(*)', &
'            integer,intent(out),optional :: get(*)', &
'', &
'CHARACTERISTICS', &
'  o  SIZE a scalar default integer', &
'', &
'  o  PUT a rank-one default integer array', &
'', &
'  o  GET a rank-one default integer array', &
'', &
'  o  the result', &
'', &
'DESCRIPTION', &
'  RANDOM_SEED(3) restarts or queries the state of the pseudorandom number', &
'  generator used by random_number.', &
'', &
'  If random_seed is called without arguments, it is seeded with random data', &
'  retrieved from the operating system.', &
'', &
'OPTIONS', &
'  o  SIZE : specifies the minimum size of the arrays used with the PUT and GET', &
'     arguments.', &
'', &
'  o  PUT : the size of the array must be larger than or equal to the number', &
'     returned by the SIZE argument.', &
'', &
'  o  GET : It is INTENT(OUT) and the size of the array must be larger than or', &
'     equal to the number returned by the SIZE argument.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_random_seed', &
'      implicit none', &
'      integer, allocatable :: seed(:)', &
'      integer :: n', &
'', &
'         call random_seed(size = n)', &
'         allocate(seed(n))', &
'         call random_seed(get=seed)', &
'         write (*, *) seed', &
'', &
'      end program demo_random_seed', &
'', &
'  Results:', &
'', &
'           -674862499 -1750483360  -183136071  -317862567   682500039', &
'           349459   344020729 -1725483289', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  RANDOM_NUMBER(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022         random_seed(3fortran)', &
'']

shortname="random_seed"
call process()


case('161','range')

textblock=[character(len=256) :: &
'', &
'range(3fortran)                                                range(3fortran)', &
'', &
'NAME', &
'  RANGE(3) - [NUMERIC MODEL] Decimal exponent range of a numeric kind', &
'', &
'SYNOPSIS', &
'  result = range(x)', &
'', &
'            integer function range (x)', &
'', &
'             TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be of type integer, real, or complex. It may be a scalar or an', &
'     array.', &
'', &
'  o  KIND is any kind supported by the type of X', &
'', &
'  o  the result is a default integer scalar', &
'', &
'DESCRIPTION', &
'  RANGE(3) returns the decimal exponent range in the model of the type of X.', &
'', &
'  Since X is only used to determine the type and kind being interrogated, the', &
'  value need not be defined.', &
'', &
'OPTIONS', &
'  o  X : the value whose type and kind are used for the query', &
'', &
'RESULT', &
'  Case (i) : For an integer argument, the result has the value', &
'', &
'          int (log10 (huge(x)))', &
'', &
'  Case (ii) : For a real argument, the result has the value', &
'', &
'           int(min (log10 (huge(x)), -log10(tiny(x) )))', &
'', &
'  Case (iii) : For a complex argument, the result has the value', &
'', &
'          range(real(x))', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_range', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32', &
'      implicit none', &
'      real(kind=sp)    :: x(2)', &
'      complex(kind=dp) :: y', &
'         print *, precision(x), range(x)', &
'         print *, precision(y), range(y)', &
'      end program demo_range', &
'', &
'  Results:', &
'', &
'       >            6          37', &
'       >           15         307', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RRSPACING(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               range(3fortran)', &
'']

shortname="range"
call process()


case('162','rank')

textblock=[character(len=256) :: &
'', &
'rank(3fortran)                                                  rank(3fortran)', &
'', &
'NAME', &
'  RANK(3) - [ARRAY:INQUIRY] Rank of a data object', &
'', &
'SYNOPSIS', &
'  result = rank(a)', &
'', &
'           integer function rank(a)', &
'', &
'            type(TYPE(kind=**)),intent(in) :: a(..)', &
'', &
'CHARACTERISTICS', &
'  o  A can be of any type TYPE and rank.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  RANK(3) returns the rank of a scalar or array data object.', &
'', &
'  The rank of an array is the number of dimensions it has (zero for a scalar).', &
'', &
'OPTIONS', &
'  o  A is the data object to query the dimensionality of. The rank returned', &
'     may be from 0 to 16.', &
'', &
'     The argument A may be any data object type, including an assumed-rank', &
'     array.', &
'', &
'RESULT', &
'  For arrays, their rank is returned; for scalars zero is returned.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_rank', &
'      implicit none', &
'', &
'      ! a bunch of data objects to query', &
'      integer           :: a', &
'      real, allocatable :: b(:,:)', &
'      real, pointer     :: c(:)', &
'      complex           :: d', &
'', &
'      ! make up a type', &
'      type mytype', &
'         integer :: int', &
'         real :: float', &
'         character :: char', &
'      end type mytype', &
'      type(mytype) :: any_thing(1,2,3,4,5)', &
'', &
'        ! basics', &
'         print *, ''rank of scalar a='',rank(a)', &
'         ! you can query this array even though it is not allocated', &
'         print *, ''rank of matrix b='',rank(b)', &
'         print *, ''rank of vector pointer c='',rank(c)', &
'         print *, ''rank of complex scalar d='',rank(d)', &
'', &
'        ! you can query any type, not just intrinsics', &
'         print *, ''rank of any arbitrary type='',rank(any_thing)', &
'', &
'        ! an assumed-rank object may be queried', &
'         call query_int(10)', &
'         call query_int([20,30])', &
'         call query_int( reshape([40,50,60,70],[2,2]) )', &
'', &
'        ! you can even query an unlimited polymorphic entity', &
'         call query_anything(10.0)', &
'         call query_anything([.true.,.false.])', &
'         call query_anything( reshape([40.0,50.0,60.0,70.0],[2,2]) )', &
'', &
'      contains', &
'', &
'      subroutine query_int(data_object)', &
'      ! It is hard to do much with something dimensioned', &
'      ! name(..) if not calling C except inside of a', &
'      ! SELECT_RANK construct but one thing you can', &
'      ! do is call the inquiry functions ...', &
'      integer,intent(in) :: data_object(..)', &
'      character(len=*),parameter :: all=''(*(g0,1x))''', &
'', &
'         if(rank(data_object).eq.0)then', &
'            print all,&', &
'            & ''passed a scalar to an assumed rank,  &', &
'            & rank='',rank(data_object)', &
'         else', &
'            print all,&', &
'            & ''passed an array to an assumed rank,  &', &
'            & rank='',rank(data_object)', &
'         endif', &
'', &
'      end subroutine query_int', &
'', &
'      subroutine query_anything(data_object)', &
'      class(*),intent(in) ::data_object(..)', &
'      character(len=*),parameter :: all=''(*(g0,1x))''', &
'        if(rank(data_object).eq.0)then', &
'          print all,&', &
'          &''passed a scalar to an unlimited polymorphic rank='', &', &
'          & rank(data_object)', &
'        else', &
'          print all,&', &
'          & ''passed an array to an unlimited polymorphic, rank='', &', &
'          & rank(data_object)', &
'        endif', &
'      end subroutine query_anything', &
'', &
'      end program demo_rank', &
'', &
'  Results:', &
'', &
'          rank of scalar a=           0', &
'          rank of matrix b=           2', &
'          rank of vector pointer c=           1', &
'          rank of complex scalar d=           0', &
'          rank of any arbitrary type=           5', &
'', &
'    passed a scalar to an assumed rank,', &
'      rank= 0', &
'', &
'    passed an array to an assumed rank,', &
'      rank= 1', &
'', &
'    passed an array to an assumed rank,', &
'      rank= 2 passed a scalar to an unlimited polymorphic rank= 0 passed an', &
'      array to an unlimited polymorphic, rank= 1 passed an array to an', &
'      unlimited polymorphic, rank= 2', &
'', &
'STANDARD', &
'SEE ALSO', &
'  Array inquiry:', &
'', &
'  o  SIZE(3) - Determine the size of an array', &
'', &
'  o  RANK(3) - Rank of a data object', &
'', &
'  o  SHAPE(3) - Determine the shape of an array', &
'', &
'  o  UBOUND(3) - Upper dimension bounds of an array', &
'', &
'  o  LBOUND(3) - Lower dimension bounds of an array', &
'', &
'  State Inquiry:', &
'', &
'  o  ALLOCATED(3) - Status of an allocatable entity', &
'', &
'  o  IS_CONTIGUOUS(3) - Test if object is contiguous', &
'', &
'  Kind Inquiry:', &
'', &
'  o  KIND(3) - Kind of an entity', &
'', &
'  Bit Inquiry:', &
'', &
'  o  STORAGE_SIZE(3) - Storage size in bits', &
'', &
'  o  BIT_SIZE(3) - Bit size inquiry function', &
'', &
'  o  BTEST(3) - Tests a bit of an integer value.', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                rank(3fortran)', &
'']

shortname="rank"
call process()


case('163','real')

textblock=[character(len=256) :: &
'', &
'real(3fortran)                                                  real(3fortran)', &
'', &
'NAME', &
'  REAL(3) - [TYPE:NUMERIC] Convert to real type', &
'', &
'SYNOPSIS', &
'  result = real(x [,kind])', &
'', &
'         elemental real(kind=KIND) function real(x,KIND)', &
'', &
'          TYPE(kind=**),intent(in) :: x', &
'          integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  the type of X may be integer, real, or complex; or a BOZ-literal-', &
'     constant.', &
'', &
'  o  KIND is a integer initialization expression (a constant expression)', &
'', &
'     o  If KIND is present it defines the kind of the real result', &
'', &
'     o  if KIND is not present', &
'', &
'        o  when X is complex the result is a real of the same kind as X.', &
'', &
'        o  when X is real or integer the result is a real of default kind', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  REAL(3) converts its argument X to a real type.', &
'', &
'  The real part of a complex value is returned. For complex values this is', &
'  similar to the modern complex-part-designator %RE which also designates the', &
'  real part of a complex value.', &
'', &
'            z=(3.0,4.0)     ! if z is a complex value', &
'            print *, z%re == real(z) ! these expressions are equivalent', &
'', &
'OPTIONS', &
'  o  X : An integer, real, or complex value to convert to real.', &
'', &
'  o  KIND : When present the value of KIND defines the kind of the result.', &
'', &
'RESULT', &
'  1.  REAL(X) converts X to a default real type if X is an integer or real', &
'      variable.', &
'', &
'  2.  REAL(X) converts a complex value to a real type with the magnitude of', &
'      the real component of the input with kind type parameter the same as X.', &
'', &
'  3.  REAL(X, KIND) is converted to a real type with kind type parameter KIND', &
'      if X is a complex, integer, or real variable.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_real', &
'      use,intrinsic :: iso_fortran_env, only : dp=>real64', &
'      implicit none', &
'      complex              :: zr = (1.0, 2.0)', &
'      doubleprecision      :: xd=huge(3.0d0)', &
'      complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)', &
'', &
'         print *, real(zr), aimag(zr)', &
'         print *, dble(zd), aimag(zd)', &
'', &
'         write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)', &
'      end program demo_real', &
'', &
'  Results:', &
'', &
'       1.00000000       2.00000000', &
'       4.0000000000000000       5.0000000000000000', &
'       1.7976931348623157E+308  1.7976931348623157E+308  1.7976931348623157E+308', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  o  AIMAG(3) - Imaginary part of complex number', &
'', &
'  o  CMPLX(3) - Complex conversion function', &
'', &
'  o  CONJG(3) - Complex conjugate function', &
'', &
'  Fortran has strong support for complex values, including many intrinsics', &
'  that take or produce complex values in addition to algebraic and logical', &
'  expressions:', &
'', &
'  ABS(3), ACOSH(3), ACOS(3), ASINH(3), ASIN(3), ATAN2(3), ATANH(3), ATAN(3),', &
'  COSH(3), COS(3), CO_SUM(3), DBLE(3), DOT_PRODUCT(3), EXP(3), INT(3),', &
'  IS_CONTIGUOUS(3), KIND(3), LOG(3), MATMUL(3), PRECISION(3), PRODUCT(3),', &
'  RANGE(3), RANK(3), SINH(3), SIN(3), SQRT(3), STORAGE_SIZE(3), SUM(3),', &
'  TANH(3), TAN(3), UNPACK(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                real(3fortran)', &
'']

shortname="real"
call process()


case('164','reduce')

textblock=[character(len=256) :: &
'', &
'reduce(3fortran)                                              reduce(3fortran)', &
'', &
'NAME', &
'  REDUCE(3) - [TRANSFORMATIONAL] General reduction of an array', &
'', &
'SYNOPSIS', &
'  There are two forms to this function:', &
'', &
'         result = reduce(array, operation [,mask]  [,identity]  [,ordered] )', &
'', &
'  or', &
'', &
'         result = reduce (array, operation, dim  &', &
'         & [,mask] [,identity] [,ordered] )', &
'', &
'          type(TYPE(kind=KIND)) function reduce &', &
'          & (array, operation, dim, mask, identity, ordered )', &
'', &
'           type(TYPE(kind=KIND)),intent(in) :: array', &
'           pure function                  :: operation', &
'           integer,intent(in),optional    :: dim', &
'           logical,optional               :: mask', &
'           type(TYPE),intent(in),optional :: identity', &
'           logical,intent(in),optional    :: ordered', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY is an array of any type', &
'', &
'  o  OPERATION is a pure function with exactly two arguments', &
'', &
'     o  each argument is scalar, non-allocatable, a nonpointer, nonpolymorphic', &
'        and nonoptional with the same type and kind as array.', &
'', &
'     o  if one argument has the asynchronous, target, or value attribute so', &
'        shall the other.', &
'', &
'  o  DIM is an integer scalar', &
'', &
'  o  MASK is a logical conformable with ARRAY', &
'', &
'  o  IDENTITY is a scalar with the same type and type parameters as ARRAY', &
'', &
'  o  ORDERED is a logical scalar', &
'', &
'  o  the result is of the same type and type parameters as ARRAY.', &
'', &
'DESCRIPTION', &
'  REDUCE(3) reduces a list of conditionally selected values from an array to a', &
'  single value by iteratively applying a binary function.', &
'', &
'  Common in functional programming, a REDUCE function applies a binary', &
'  operator (a pure function with two arguments) to all elements cumulatively.', &
'', &
'  REDUCE is a "higher-order" function; ie. it is a function that receives', &
'  other functions as arguments.', &
'', &
'  The REDUCE function receives a binary operator (a function with two', &
'  arguments, just like the basic arithmetic operators). It is first applied to', &
'  two unused values in the list to generate an accumulator value which is', &
'  subsequently used as the first argument to the function as the function is', &
'  recursively applied to all the remaining selected values in the input array.', &
'', &
'OPTIONS', &
'  o  ARRAY : An array of any type and allowed rank to select values from.', &
'', &
'  o  OPERATION : shall be a pure function with exactly two arguments; each', &
'     argument shall be a scalar, nonallocatable, nonpointer, nonpolymorphic,', &
'     nonoptional dummy data object with the same type and type parameters as', &
'     ARRAY. If one argument has the ASYNCHRONOUS, TARGET, or VALUE attribute,', &
'     the other shall have that attribute. Its result shall be a nonpolymorphic', &
'     scalar and have the same type and type parameters as ARRAY. OPERATION', &
'     should implement a mathematically associative operation. It need not be', &
'     commutative.', &
'', &
'   NOTE', &
'  If OPERATION is not computationally associative, REDUCE without', &
'  ORDERED=.TRUE. with the same argument values might not always produce the', &
'  same result, as the processor can apply the associative law to the', &
'  evaluation.', &
'', &
'  Many operations that mathematically are associative are not when applied to', &
'  floating-point numbers. The order you sum values in may affect the result,', &
'  for example.', &
'', &
'  o  DIM : An integer scalar with a value in the range 1<= DIM <= n, where n', &
'     is the rank of ARRAY.', &
'', &
'     o  MASK : (optional) shall be of type logical and shall be conformable', &
'        with ARRAY.', &
'', &
'        When present only those elements of ARRAY are passed to OPERATION for', &
'        which the corresponding elements of MASK are true, as if *array was', &
'        filtered with PACK(3).', &
'', &
'     o  IDENTITY : shall be scalar with the same type and type parameters as', &
'        ARRAY. If the initial sequence is empty, the result has the value', &
'        IDENTIFY if IDENTIFY is present, and otherwise, error termination is', &
'        initiated.', &
'', &
'     o  ORDERED : shall be a logical scalar. If ORDERED is present with the', &
'        value .true., the calls to the OPERATOR function begins with the first', &
'        two elements of ARRAY and the process continues in row-column order', &
'        until the sequence has only one element which is the value of the', &
'        reduction. Otherwise, the compiler is free to assume that the', &
'        operation is commutative and may evaluate the reduction in the most', &
'        optimal way.', &
'', &
'RESULT', &
'  The result is of the same type and type parameters as ARRAY. It is scalar if', &
'  DIM does not appear.', &
'', &
'  If DIM is present, it indicates the one dimension along which to perform the', &
'  reduction, and the resultant array has a rank reduced by one relative to the', &
'  input array.', &
'', &
'EXAMPLES', &
'  The following examples all use the function MY_MULT, which returns the', &
'  product of its two real arguments.', &
'', &
'         program demo_reduce', &
'         implicit none', &
'         character(len=*),parameter :: f=''("[",*(g0,",",1x),"]")''', &
'         integer,allocatable :: arr(:), b(:,:)', &
'', &
'         ! Basic usage:', &
'            ! the product of the elements of an array', &
'            arr=[1, 2, 3, 4 ]', &
'            write(*,*) arr', &
'            write(*,*) ''product='', reduce(arr, my_mult)', &
'            write(*,*) ''sum='', reduce(arr, my_sum)', &
'', &
'         ! Examples of masking:', &
'            ! the product of only the positive elements of an array', &
'            arr=[1, -1, 2, -2, 3, -3 ]', &
'            write(*,*)''positive value product='',reduce(arr, my_mult, mask=arr>0)', &
'         ! sum values ignoring negative values', &
'            write(*,*)''sum positive values='',reduce(arr, my_sum, mask=arr>0)', &
'', &
'         ! a single-valued array returns the single value as the', &
'         ! calls to the operator stop when only one element remains', &
'            arr=[ 1234 ]', &
'            write(*,*)''single value sum'',reduce(arr, my_sum )', &
'            write(*,*)''single value product'',reduce(arr, my_mult )', &
'', &
'         ! Example of operations along a dimension:', &
'         !  If B is the array   1 3 5', &
'         !                      2 4 6', &
'            b=reshape([1,2,3,4,5,6],[2,3])', &
'            write(*,f) REDUCE(B, MY_MULT),''should be [720]''', &
'            write(*,f) REDUCE(B, MY_MULT, DIM=1),''should be [2,12,30]''', &
'            write(*,f) REDUCE(B, MY_MULT, DIM=2),''should be [15, 48]''', &
'', &
'         contains', &
'', &
'         pure function my_mult(a,b) result(c)', &
'         integer,intent(in) :: a, b', &
'         integer            :: c', &
'            c=a*b', &
'         end function my_mult', &
'', &
'         pure function my_sum(a,b) result(c)', &
'         integer,intent(in) :: a, b', &
'         integer            :: c', &
'            c=a+b', &
'         end function my_sum', &
'', &
'         end program demo_reduce', &
'', &
'  Results:', &
'', &
'           >  1 2 3 4', &
'           >  product= 24', &
'           >  sum=     10', &
'           >  positive value sum= 6', &
'           >  sum positive values= 6', &
'           >  single value sum     1234', &
'           >  single value product 1234', &
'           > [720, should be [720],', &
'           > [2, 12, 30, should be [2,12,30],', &
'           > [15, 48, should be [15, 48],', &
'', &
'STANDARD', &
'  Fortran 2018', &
'', &
'SEE ALSO', &
'  o  co_reduce(3)', &
'', &
'RESOURCES', &
'  o  associative:wikipedia', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              reduce(3fortran)', &
'']

shortname="reduce"
call process()


case('165','repeat')

textblock=[character(len=256) :: &
'', &
'repeat(3fortran)                                              repeat(3fortran)', &
'', &
'NAME', &
'  REPEAT(3) - [CHARACTER] Repeated string concatenation', &
'', &
'SYNOPSIS', &
'  result = repeat(string, ncopies)', &
'', &
'           character(len=len(string)*ncopies) function repeat(string, ncopies)', &
'', &
'            character(len=*),intent(in)   :: string', &
'            integer(kind=**),intent(in)   :: ncopies', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  STRING is a scalar character type.', &
'', &
'  o  NCOPIES is a scalar integer.', &
'', &
'  o  the result is a new scalar of type character of the same kind as', &
'', &
'   STRING', &
'DESCRIPTION', &
'  REPEAT(3) concatenates copies of a string.', &
'', &
'OPTIONS', &
'  o  STRING : The input string to repeat', &
'', &
'  o  NCOPIES : Number of copies to make of STRING, greater than or equal to', &
'     zero (0).', &
'', &
'RESULT', &
'  A new string built up from NCOPIES copies of STRING.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_repeat', &
'      implicit none', &
'          write(*,''(a)'') repeat("^v", 35)         ! line break', &
'          write(*,''(a)'') repeat("_", 70)          ! line break', &
'          write(*,''(a)'') repeat("1234567890", 7)  ! number line', &
'          write(*,''(a)'') repeat("         |", 7)  !', &
'      end program demo_repeat', &
'', &
'  Results:', &
'', &
'       > ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v', &
'       > ______________________________________________________________________', &
'       > 1234567890123456789012345678901234567890123456789012345678901234567890', &
'       >          |         |         |         |         |         |         |', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  Functions that perform operations on character strings:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NON-ELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              repeat(3fortran)', &
'']

shortname="repeat"
call process()


case('166','reshape')

textblock=[character(len=256) :: &
'', &
'reshape(3fortran)                                            reshape(3fortran)', &
'', &
'              reshape', &
'', &
'NAME', &
'  RESHAPE(3) - [ARRAY:RESHAPE] Function to reshape an array', &
'', &
'SYNOPSIS', &
'  result = reshape( source, shape [,pad] [,order] )', &
'', &
'           type(TYPE(kind=KIND) function reshape', &
'', &
'            type(TYPE(kind=KIND),intent(in)          :: source(..)', &
'            integer(kind=**),intent(in)              :: shape(:)', &
'            type(TYPE(kind=KIND),intent(in),optional :: pad(..)', &
'            integer(kind=**),intent(in),optional     :: order(:)', &
'', &
'CHARACTERISTICS', &
'  o  SOURCE is an array of any type', &
'', &
'  o  SHAPE defines a Fortran shape and therefore an integer vector (of rank', &
'     one) of constant size of up to 16 non-negative values.', &
'', &
'  o  PAD is the same type as SOURCE', &
'', &
'  o  ORDER is the same shape as SHAPE', &
'', &
'  o  The result is an array of shape SHAPE with the same type as SOURCE.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  RESHAPE constructs an array of arbitrary shape SHAPE using the elements from', &
'  SOURCE and possibly PAD to fill it.', &
'', &
'  If necessary, the new array may be padded with elements from PAD or permuted', &
'  as defined by ORDER.', &
'', &
'  Among many other uses, RESHAPE can be used to reorder a Fortran array to', &
'  match C array ordering before the array is passed from Fortran to a C', &
'  procedure.', &
'', &
'OPTIONS', &
'  o  SOURCE : an array containing the elements to be copied to the result.', &
'     there must be enough elements in the source to fill the new shape if PAD', &
'     is omitted or has size zero. Expressed in Fortran ...', &
'', &
'         if(.not.present(pad))then', &
'            if(size(source) < product(shape))then', &
'              stop ''not enough elements in the old array to fill the new one''', &
'            endif', &
'         endif', &
'', &
'  o  SHAPE : This is the shape of the new array being generated. Being by', &
'     definition a shape; all elements are either positive integers or zero,', &
'     the size but be 1 or greater, it may have up to 16 elements but must be', &
'     of constant fixed size and rank one.', &
'', &
'  o  PAD : used to fill in extra values if the result array is larger than', &
'     SOURCE. It will be used repeatedly after all the elements of SOURCE have', &
'     been placed in the result until the result has all elements assigned. :', &
'     If it is absent or is a zero-sized array, you can only make SOURCE into', &
'     another array of the same size as SOURCE or smaller.', &
'', &
'  o  ORDER : used to insert elements in the result in an order other than the', &
'     normal Fortran array element order, in which the first dimension varies', &
'     fastest. : By definition of ranks the values have to be a permutation of', &
'     the numbers from 1 to n, where n is the rank of SHAPE. : the elements of', &
'     SOURCE and pad are placed into the result in order; changing the left-', &
'     most rank most rapidly by default. To change the order by which the', &
'     elements are placed in the result use ORDER.', &
'', &
'RESULT', &
'  The result is an array of shape SHAPE with the same type and type parameters', &
'  as SOURCE. It is first filled with the values of elements of SOURCE, with', &
'  the remainder filled with repeated copies of PAD until all elements are', &
'  filled. The new array may be smaller than SOURCE.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_reshape', &
'      implicit none', &
'      ! notice the use of "shape(box)" on the RHS', &
'      integer :: box(3,4)=reshape([1,2,3,4,5,6,7,8,9,10,11,12],shape(box))', &
'      integer,allocatable :: v(:,:)', &
'      integer :: rc(2)', &
'         ! basics0', &
'          ! what is the current shape of the array?', &
'          call printi(''shape of box is '',box)', &
'          ! change the shape', &
'          call printi(''reshaped '',reshape(box,[2,6]))', &
'          call printi(''reshaped '',reshape(box,[4,3]))', &
'', &
'         ! fill in row column order using order', &
'          v=reshape([1,2,3,4,10,20,30,40,100,200,300,400],[1,12])', &
'          call printi(''here is some data to shape'',v)', &
'          call printi(''normally fills columns first '',reshape([v],[3,4]))', &
'          call printi(''fill rows first'', reshape([v],[3,4],order=[2,1]))', &
'', &
'          ! if we take the data and put in back in filling', &
'          ! rows first instead of columns, and flipping the', &
'          ! height and width of the box we not only fill in', &
'          ! a vector using row-column order we actually', &
'          ! transpose it.', &
'          rc(2:1:-1)=shape(box)', &
'          ! copy the data in changing column number fastest', &
'          v=reshape(box,rc,order=[2,1])', &
'          call printi(''reshaped and reordered'',v)', &
'          ! of course we could have just done a transpose', &
'          call printi(''transposed'',transpose(box))', &
'', &
'         ! making the result bigger than source using pad', &
'          v=reshape(box,rc*2,pad=[-1,-2,-3],order=[2,1])', &
'          call printi(''bigger and padded and reordered'',v)', &
'      contains', &
'', &
'      subroutine printi(title,arr)', &
'      implicit none', &
'', &
'      !@(#) print small 2d integer arrays in row-column format', &
'', &
'      character(len=*),parameter :: all=''(*(g0,1x))'' ! a handy format', &
'      character(len=*),intent(in)  :: title', &
'      integer,intent(in)           :: arr(:,:)', &
'      integer                      :: i', &
'      character(len=:),allocatable :: biggest', &
'', &
'         print all', &
'         print all, trim(title),'':('',shape(arr),'')''  ! print title', &
'         biggest=''           ''  ! make buffer to write integer into', &
'         ! find how many characters to use for integers', &
'         write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'         ! use this format to write a row', &
'         biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'         ! print one row of array at a time', &
'         do i=1,size(arr,dim=1)', &
'            write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'            write(*,''(" ]")'')', &
'         enddo', &
'', &
'      end subroutine printi', &
'', &
'      end program demo_reshape', &
'', &
'  Results:', &
'', &
'         shape of box is :( 3 4 )', &
'          > [   1,   4,   7,  10 ]', &
'          > [   2,   5,   8,  11 ]', &
'          > [   3,   6,   9,  12 ]', &
'', &
'         reshaped :( 2 6 )', &
'          > [   1,   3,   5,   7,   9,  11 ]', &
'          > [   2,   4,   6,   8,  10,  12 ]', &
'', &
'         reshaped :( 4 3 )', &
'          > [   1,   5,   9 ]', &
'          > [   2,   6,  10 ]', &
'          > [   3,   7,  11 ]', &
'          > [   4,   8,  12 ]', &
'', &
'         here is some data to shape :( 1 12 )', &
'          > [   1,   2,   3,   4,  10,  20,  30,  40, 100, 200, 300, 400 ]', &
'', &
'         normally fills columns first :( 3 4 )', &
'          > [    1,    4,   30,  200 ]', &
'          > [    2,   10,   40,  300 ]', &
'          > [    3,   20,  100,  400 ]', &
'', &
'         fill rows first :( 3 4 )', &
'          > [    1,    2,    3,    4 ]', &
'          > [   10,   20,   30,   40 ]', &
'          > [  100,  200,  300,  400 ]', &
'', &
'         reshaped and reordered :( 4 3 )', &
'          > [   1,   2,   3 ]', &
'          > [   4,   5,   6 ]', &
'          > [   7,   8,   9 ]', &
'          > [  10,  11,  12 ]', &
'', &
'         transposed :( 4 3 )', &
'          > [   1,   2,   3 ]', &
'          > [   4,   5,   6 ]', &
'          > [   7,   8,   9 ]', &
'          > [  10,  11,  12 ]', &
'', &
'         bigger and padded and reordered :( 8 6 )', &
'          > [   1,   2,   3,   4,   5,   6 ]', &
'          > [   7,   8,   9,  10,  11,  12 ]', &
'          > [  -1,  -2,  -3,  -1,  -2,  -3 ]', &
'          > [  -1,  -2,  -3,  -1,  -2,  -3 ]', &
'          > [  -1,  -2,  -3,  -1,  -2,  -3 ]', &
'          > [  -1,  -2,  -3,  -1,  -2,  -3 ]', &
'          > [  -1,  -2,  -3,  -1,  -2,  -3 ]', &
'          > [  -1,  -2,  -3,  -1,  -2,  -3 ]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  SHAPE(3), PACK(3), TRANSPOSE(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             reshape(3fortran)', &
'']

shortname="reshape"
call process()


case('167','rrspacing')

textblock=[character(len=256) :: &
'', &
'rrspacing(3fortran)                                        rrspacing(3fortran)', &
'', &
'NAME', &
'  RRSPACING(3) - [MODEL_COMPONENTS] Reciprocal of the relative spacing of a', &
'  numeric type', &
'', &
'SYNOPSIS', &
'  result = rrspacing(x)', &
'', &
'           elemental real(kind=KIND) function rrspacing(x)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is type real an any kind', &
'', &
'  o  The return value is of the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  RRSPACING(3) returns the reciprocal of the relative spacing of model numbers', &
'  near X.', &
'', &
'OPTIONS', &
'  o  X : Shall be of type real.', &
'', &
'RESULT', &
'  The return value is of the same type and kind as X. The value returned is', &
'  equal to ABS(FRACTION(X)) * FLOAT(RADIX(X))**DIGITS(X).', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), SCALE(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022           rrspacing(3fortran)', &
'']

shortname="rrspacing"
call process()


case('168','same_type_as')

textblock=[character(len=256) :: &
'', &
'same_type_as(3fortran)                                  same_type_as(3fortran)', &
'', &
'NAME', &
'  SAME_TYPE_AS(3) - [STATE:INQUIRY] Query dynamic types for equality', &
'', &
'SYNOPSIS', &
'  result = same_type_as(a, b)', &
'', &
'           logical same_type_as(a, b)', &
'', &
'            type(TYPE(kind=KIND),intent(in) :: a', &
'            type(TYPE(kind=KIND),intent(in) :: b', &
'', &
'CHARACTERISTICS', &
'  o  A shall be an object of extensible declared type or unlimited', &
'     polymorphic. If it is a polymorphic pointer, it shall not have an', &
'     undefined association status.', &
'', &
'  o  B shall be an object of extensible declared type or unlimited', &
'     polymorphic. If it is a polymorphic pointer, it shall not have an', &
'     undefined association status.', &
'', &
'DESCRIPTION', &
'  SAME_TYPE_AS(3) queries the dynamic types of objects for equality.', &
'', &
'OPTIONS', &
'  o  A : object to compare to B for equality of type', &
'', &
'  o  B : object to be compared to for equality of type', &
'', &
'RESULT', &
'  If the dynamic type of A or B is extensible, the result is true if and only', &
'  if the dynamic type of A is the same as the dynamic type of B. If neither A', &
'  nor B has extensible dynamic type, the result is processor dependent.', &
'', &
'      NOTE1', &
'', &
'  The dynamic type of a disassociated pointer or unallocated allocatable', &
'  variable is its declared type. An unlimited polymorphic entity has no', &
'  declared type.', &
'', &
'      NOTE2', &
'', &
'  The test performed by SAME_TYPE_AS is not the same as the test performed by', &
'  the type guard TYPE IS. The test performed by SAME_TYPE_AS does not consider', &
'  kind type parameters.', &
'', &
'  Sample program:', &
'', &
'        ! program demo_same_type_as', &
'        module M_ether', &
'        implicit none', &
'        private', &
'', &
'        type   :: dot', &
'          real :: x=0', &
'          real :: y=0', &
'        end type dot', &
'', &
'        type, extends(dot) :: point', &
'          real :: z=0', &
'        end type point', &
'', &
'        type something_else', &
'        end type something_else', &
'', &
'        public :: dot', &
'        public :: point', &
'        public :: something_else', &
'', &
'        end module M_ether', &
'', &
'        program demo_same_type_as', &
'        use M_ether, only : dot, point, something_else', &
'        implicit none', &
'        type(dot) :: dad, mom', &
'        type(point) :: me', &
'        type(something_else) :: alien', &
'', &
'         write(*,*)same_type_as(me,dad),''I am descended from Dad, but equal?''', &
'         write(*,*)same_type_as(me,me) ,''I am what I am''', &
'         write(*,*)same_type_as(dad,mom) ,''what a pair!''', &
'', &
'         write(*,*)same_type_as(dad,me),''no paradox here''', &
'         write(*,*)same_type_as(dad,alien),''no relation''', &
'', &
'         call pointers()', &
'         contains', &
'         subroutine pointers()', &
'         ! Given the declarations and assignments', &
'         type t1', &
'            real c', &
'         end type', &
'         type, extends(t1) :: t2', &
'         end type', &
'         class(t1), pointer :: p, q, r', &
'            allocate (p, q)', &
'            allocate (t2 :: r)', &
'            ! the result of SAME_TYPE_AS (P, Q) will be true, and the result', &
'            ! of SAME_TYPE_AS (P, R) will be false.', &
'            write(*,*)''(P,Q)'',same_type_as(p,q),"mind your P''s and Q''s"', &
'            write(*,*)''(P,R)'',same_type_as(p,r)', &
'         end subroutine pointers', &
'', &
'        end program demo_same_type_as', &
'', &
'  Results:', &
'', &
'          F I am descended from Dad, but equal?', &
'          T I am what I am', &
'          T what a pair!', &
'          F no paradox here', &
'          F no relation', &
'          (P,Q) T mind your P''s and Q''s', &
'          (P,R) F', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  EXTENDS_TYPE_OF(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022        same_type_as(3fortran)', &
'']

shortname="same_type_as"
call process()


case('169','scale')

textblock=[character(len=256) :: &
'', &
'scale(3fortran)                                                scale(3fortran)', &
'', &
'NAME', &
'  SCALE(3) - [MODEL_COMPONENTS] Scale a real value by a whole power of the', &
'  radix', &
'', &
'SYNOPSIS', &
'  result = scale(x, i)', &
'', &
'           elemental real(kind=KIND) function scale(x, i)', &
'', &
'            real(kind=KIND),intent(in)   :: x', &
'            integer(kind=**),intent(in)  :: i', &
'', &
'CHARACTERISTICS', &
'  o  X is type real of any kind', &
'', &
'  o  I is type an integer of any kind', &
'', &
'  o  the result is real of the same kind as X', &
'', &
'DESCRIPTION', &
'  SCALE(3) returns x * RADIX(X)**I.', &
'', &
'  It is almost certain the radix(base) of the platform is two, therefore', &
'  SCALE(3) is generally the same as X*2**I', &
'', &
'OPTIONS', &
'  o  X : the value to multiply by RADIX(X)**I. Its type and kind is used to', &
'     determine the radix for values with its characteristics and determines', &
'     the characteristics of the result, so care must be taken the returned', &
'     value is within the range of the characteristics of X.', &
'', &
'  o  I : The power to raise the radix of the machine to', &
'', &
'RESULT', &
'  The return value is X * RADIX(X)**I, assuming that value can be represented', &
'  by a value of the type and kind of X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_scale', &
'      implicit none', &
'      real :: x = 178.1387e-4', &
'      integer :: i = 5', &
'         print *, scale(x,i), x*radix(x)**i', &
'      end program demo_scale', &
'', &
'  Results:', &
'', &
'          0.570043862      0.570043862', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SET_EXPONENT(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               scale(3fortran)', &
'']

shortname="scale"
call process()


case('170','scan')

textblock=[character(len=256) :: &
'', &
'scan(3fortran)                                                  scan(3fortran)', &
'', &
'NAME', &
'  SCAN(3) - [CHARACTER:SEARCH] Scan a string for the presence of a set of', &
'  characters', &
'', &
'SYNOPSIS', &
'  result = scan( string, set, [,back] [,kind] )', &
'', &
'           elemental integer(kind=KIND) function scan(string,set,back,kind)', &
'', &
'            character(len=*,kind=**),intent(in) :: string', &
'            character(len=*,kind=**),intent(in) :: set', &
'            logical,intent(in),optional :: back', &
'            integer,intent(in),optional :: kind', &
'', &
'CHARACTERISTICS', &
'  o  STRING is a character string of any kind', &
'', &
'  o  SET must be a character string with the same kind as STRING', &
'', &
'  o  BACK is a logical', &
'', &
'  o  KIND is a scalar integer constant expression', &
'', &
'  o  the result is an integer with the kind specified by KIND. If KIND is not', &
'     present the result is a default integer.', &
'', &
'DESCRIPTION', &
'  SCAN(3) scans a STRING for any of the characters in a SET of characters.', &
'', &
'  If BACK is either absent or equals .false., this function returns the', &
'  position of the leftmost character of STRING that is in SET. If BACK equals', &
'  .true., the rightmost position is returned. If no character of SET is found', &
'  in STRING, the result is zero.', &
'', &
'OPTIONS', &
'  o  STRING : the string to be scanned', &
'', &
'  o  SET : the set of characters which will be matched', &
'', &
'  o  BACK : if .true. the position of the rightmost character matched is', &
'     returned, instead of the leftmost.', &
'', &
'  o  KIND : the kind of the returned value is the same as KIND if present.', &
'     Otherwise a default integer kind is returned.', &
'', &
'RESULT', &
'  If BACK is absent or is present with the value false and if STRING contains', &
'  at least one character that is in SET, the value of the result is the', &
'  position of the leftmost character of STRING that is in SET.', &
'', &
'  If BACK is present with the value true and if STRING contains at least one', &
'  character that is in SET, the value of the result is the position of the', &
'  rightmost character of STRING that is in SET.', &
'', &
'  The value of the result is zero if no character of STRING is in SET or if', &
'  the length of STRING or SET is zero.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_scan', &
'      implicit none', &
'         write(*,*) scan("fortran", "ao")          ! 2, found ''o''', &
'         write(*,*) scan("fortran", "ao", .true.)  ! 6, found ''a''', &
'         write(*,*) scan("fortran", "c++")         ! 0, found none', &
'      end program demo_scan', &
'', &
'  Results:', &
'', &
'       >            2', &
'       >            6', &
'       >            0', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                scan(3fortran)', &
'']

shortname="scan"
call process()


case('171','selected_char_kind')

textblock=[character(len=256) :: &
'', &
'selected_char_kind(3fortran)                      selected_char_kind(3fortran)', &
'', &
'NAME', &
'  SELECTED_CHAR_KIND(3) - [KIND] Select character kind such as "Unicode"', &
'', &
'SYNOPSIS', &
'  result = selected_char_kind(name)', &
'', &
'           integer function selected_char_kind(name)', &
'', &
'            character(len=*),intent(in) :: name', &
'', &
'CHARACTERISTICS', &
'  o  NAME is a default character scalar', &
'', &
'  o  the result is a default integer scalar', &
'', &
'DESCRIPTION', &
'  SELECTED_CHAR_KIND(3) returns a kind parameter value for the character set', &
'  named NAME.', &
'', &
'  If a name is not supported, -1 is returned. Otherwise the result is a value', &
'  equal to that kind type parameter value.', &
'', &
'  The list of supported names is processor-dependent except for "DEFAULT".', &
'', &
'  o  If NAME has the value "DEFAULT", then the result has a value equal to', &
'     that of the kind type parameter of default character. This name is always', &
'     supported.', &
'', &
'  o  If NAME has the value "ASCII", then the result has a value equal to that', &
'     of the kind type parameter of ASCII character.', &
'', &
'  o  If NAME has the value "ISO_10646", then the result has a value equal to', &
'     that of the kind type parameter of the ISO 10646 character kind', &
'     (corresponding to UCS-4 as specified in ISO/IEC 10646).', &
'', &
'  o  If NAME is a processor-defined name of some other character kind', &
'     supported by the processor, then the result has a value equal to that', &
'     kind type parameter value. Pre-defined names include "ASCII" and', &
'     "ISO_10646".', &
'', &
'  The NAME is interpreted without respect to case or trailing blanks.', &
'', &
'OPTIONS', &
'  o  NAME : A name to query the processor-dependent kind value of, and/or to', &
'     determine if supported. NAME, interpreted without respect to case or', &
'     trailing blanks.', &
'', &
'     Currently, supported character sets include "ASCII" and "DEFAULT" and', &
'     "ISO_10646" (Universal Character Set, UCS-4) which is commonly known as', &
'     "Unicode". Supported names other than "DEFAULT" are processor dependent.', &
'', &
'RESULT', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      Linux', &
'      program demo_selected_char_kind', &
'      use iso_fortran_env', &
'      implicit none', &
'', &
'      intrinsic date_and_time,selected_char_kind', &
'', &
'      ! set some aliases for common character kinds', &
'      ! as the numbers can vary from platform to platform', &
'', &
'      integer, parameter :: default = selected_char_kind ("default")', &
'      integer, parameter :: ascii =   selected_char_kind ("ascii")', &
'      integer, parameter :: ucs4  =   selected_char_kind (''ISO_10646'')', &
'      integer, parameter :: utf8  =   selected_char_kind (''utf-8'')', &
'', &
'      ! assuming ASCII and UCS4 are supported (ie. not equal to -1)', &
'      ! define some string variables', &
'      character(len=26, kind=ascii ) :: alphabet', &
'      character(len=30, kind=ucs4  ) :: hello_world', &
'      character(len=30, kind=ucs4  ) :: string', &
'', &
'         write(*,*)''ASCII     '',&', &
'          & merge(''Supported    '',''Not Supported'',ascii /= -1)', &
'         write(*,*)''ISO_10646 '',&', &
'          & merge(''Supported    '',''Not Supported'',ucs4 /= -1)', &
'         write(*,*)''UTF-8     '',&', &
'          & merge(''Supported    '',''Not Supported'',utf8 /= -1)', &
'', &
'         if(default.eq.ascii)then', &
'             write(*,*)''ASCII is the default on this processor''', &
'         endif', &
'', &
'        ! for constants the kind precedes the value, somewhat like a', &
'        ! BOZ constant', &
'         alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"', &
'         write (*,*) alphabet', &
'', &
'         hello_world = ucs4_''Hello World and Ni Hao -- '' &', &
'                       // char (int (z''4F60''), ucs4)     &', &
'                       // char (int (z''597D''), ucs4)', &
'', &
'        ! an encoding option is required on OPEN for non-default I/O', &
'         if(ucs4 /= -1 )then', &
'            open (output_unit, encoding=''UTF-8'')', &
'            write (*,*) trim (hello_world)', &
'         else', &
'            write (*,*) ''cannot use utf-8''', &
'         endif', &
'', &
'         call create_date_string(string)', &
'         write (*,*) trim (string)', &
'', &
'      contains', &
'', &
'      ! The following produces a Japanese date stamp.', &
'      subroutine create_date_string(string)', &
'      intrinsic date_and_time,selected_char_kind', &
'      integer,parameter :: ucs4 = selected_char_kind("ISO_10646")', &
'      character(len=1,kind=ucs4),parameter :: &', &
'             nen =   char(int( z''5e74'' ),ucs4), & ! year', &
'             gatsu = char(int( z''6708'' ),ucs4), & ! month', &
'             nichi = char(int( z''65e5'' ),ucs4)    ! day', &
'      character(len= *, kind= ucs4) string', &
'      integer values(8)', &
'         call date_and_time(values=values)', &
'         write(string,101) values(1),nen,values(2),gatsu,values(3),nichi', &
'       101 format(*(i0,a))', &
'      end subroutine create_date_string', &
'', &
'      end program demo_selected_char_kind', &
'', &
'  Results:', &
'', &
'  The results are very processor-dependent', &
'', &
'       >  ASCII     Supported', &
'       >  ISO_10646 Supported', &
'       >  UTF-8     Not Supported', &
'       >  ASCII is the default on this processor', &
'       >  abcdefghijklmnopqrstuvwxyz', &
'       >  Hello World and Ni Hao --', &
'       >  20221015', &
'', &
'STANDARD', &
'  Fortran 2003', &
'', &
'SEE ALSO', &
'  SELECTED_INT_KIND(3), SELECTED_REAL_KIND(3)', &
'', &
'  ACHAR(3), CHAR(3), ICHAR(3), IACHAR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022  selected_char_kind(3fortran)', &
'']

shortname="selected_char_kind"
call process()


case('172','selected_int_kind')

textblock=[character(len=256) :: &
'', &
'selected_int_kind(3fortran)                        selected_int_kind(3fortran)', &
'', &
'NAME', &
'  SELECTED_INT_KIND(3) - [KIND] Choose integer kind', &
'', &
'SYNOPSIS', &
'  result = selected_int_kind(r)', &
'', &
'  integer function selected_int_kind(r)', &
'', &
'           integer(kind=KIND),intent(in) :: r', &
'', &
'CHARACTERISTICS', &
'  o  R is an integer scalar.', &
'', &
'  o  the result is an default integer scalar.', &
'', &
'DESCRIPTION', &
'  SELECTED_INT_KIND(3) return the kind value of the smallest integer type that', &
'  can represent all values ranging from -10**R (exclusive) to 10**R', &
'  (exclusive). If there is no integer kind that accommodates this range,', &
'  selected_int_kind returns -1.', &
'', &
'OPTIONS', &
'  o  R : The value specifies the required range of powers of ten that need', &
'     supported by the kind type being returned.', &
'', &
'RESULT', &
'  The result has a value equal to the value of the kind type parameter of an', &
'  integer type that represents all values in the requested range.', &
'', &
'  if no such kind type parameter is available on the processor, the result is', &
'  -1.', &
'', &
'  If more than one kind type parameter meets the criterion, the value returned', &
'  is the one with the smallest decimal exponent range, unless there are', &
'  several such values, in which case the smallest of these kind values is', &
'  returned.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_selected_int_kind', &
'      implicit none', &
'      integer,parameter :: k5 = selected_int_kind(5)', &
'      integer,parameter :: k15 = selected_int_kind(15)', &
'      integer(kind=k5) :: i5', &
'      integer(kind=k15) :: i15', &
'', &
'          print *, huge(i5), huge(i15)', &
'', &
'          ! the following inequalities are always true', &
'          print *, huge(i5) >= 10_k5**5-1', &
'          print *, huge(i15) >= 10_k15**15-1', &
'      end program demo_selected_int_kind', &
'', &
'  Results:', &
'', &
'        >   2147483647  9223372036854775807', &
'        >  T', &
'        >  T', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  AINT(3), ANINT(3), INT(3), NINT(3), CEILING(3), FLOOR(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022   selected_int_kind(3fortran)', &
'']

shortname="selected_int_kind"
call process()


case('173','selected_real_kind')

textblock=[character(len=256) :: &
'', &
'selected_real_kind(3fortran)                      selected_real_kind(3fortran)', &
'', &
'NAME', &
'  SELECTED_REAL_KIND(3) - [KIND] Choose real kind', &
'', &
'SYNOPSIS', &
'  result = selected_real_kind([p] [,r] [,radix] )', &
'', &
'  integer function selected_int_kind(r)', &
'', &
'           real(kind=KIND),intent(in),optional :: p', &
'           real(kind=KIND),intent(in),optional :: r', &
'           real(kind=KIND),intent(in),optional :: radix', &
'', &
'CHARACTERISTICS', &
'  o  P is an integer scalar', &
'', &
'  o  R is an integer scalar', &
'', &
'  o  RADIX is an integer scalar', &
'', &
'  o  the result is an default integer scalar', &
'', &
'DESCRIPTION', &
'  SELECTED_REAL_KIND(3) return the kind value of a real data type with decimal', &
'  precision of at least P digits, exponent range of at least R, and with a', &
'  radix of RADIX. That is, if such a kind exists', &
'', &
'      + it has the decimal precision as returned by **precision**(3) of at', &
'        least **p** digits.', &
'      + a decimal exponent range, as returned by the function **range**(3)', &
'        of at least **r**', &
'      + a radix, as returned by the function **radix**(3) , of **radix**,', &
'', &
'  If the requested kind does not exist, -1 is returned.', &
'', &
'  At least one argument shall be present.', &
'', &
'OPTIONS', &
'  o  P : the requested precision', &
'', &
'  o  R : the requested range', &
'', &
'  o  RADIX : the desired radix', &
'', &
'      Before FORTRAN 2008, at least one of the arguments R or P shall be', &
'      present; since FORTRAN 2008, they are assumed to be zero if absent.', &
'', &
'RESULT', &
'  selected_real_kind returns the value of the kind type parameter of a real', &
'  data type with decimal precision of at least P digits, a decimal exponent', &
'  range of at least R, and with the requested RADIX.', &
'', &
'  If P or R is absent, the result value is the same as if it were present with', &
'  the value zero.', &
'', &
'  If the RADIX parameter is absent, there is no requirement on the radix of', &
'  the selected kind and real kinds with any radix can be returned.', &
'', &
'  If more than one real data type meet the criteria, the kind of the data type', &
'  with the smallest decimal precision is returned. If no real data type', &
'  matches the criteria, the result is', &
'', &
'  o  -1 : if the processor does not support a real data type with a precision', &
'     greater than or equal to P, but the R and RADIX requirements can be', &
'     fulfilled', &
'', &
'  o  -2 : if the processor does not support a real type with an exponent range', &
'     greater than or equal to R, but P and RADIX are fulfillable', &
'', &
'  o  -3 : if RADIX but not P and R requirements are fulfillable', &
'', &
'  o  -4 : if RADIX and either P or R requirements are fulfillable', &
'', &
'  o  -5 : if there is no real type with the given RADIX', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_selected_real_kind', &
'      implicit none', &
'      integer,parameter :: p6 = selected_real_kind(6)', &
'      integer,parameter :: p10r100 = selected_real_kind(10,100)', &
'      integer,parameter :: r400 = selected_real_kind(r=400)', &
'      real(kind=p6) :: x', &
'      real(kind=p10r100) :: y', &
'      real(kind=r400) :: z', &
'', &
'         print *, precision(x), range(x)', &
'         print *, precision(y), range(y)', &
'         print *, precision(z), range(z)', &
'      end program demo_selected_real_kind', &
'', &
'  Results:', &
'', &
'        >            6          37', &
'        >           15         307', &
'        >           18        4931', &
'', &
'STANDARD', &
'  Fortran 95 ; with RADIX - Fortran 2008', &
'', &
'SEE ALSO', &
'  PRECISION(3), RANGE(3), RADIX(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022  selected_real_kind(3fortran)', &
'']

shortname="selected_real_kind"
call process()


case('174','set_exponent')

textblock=[character(len=256) :: &
'', &
'set_exponent(3fortran)                                  set_exponent(3fortran)', &
'', &
'NAME', &
'  SET_EXPONENT(3) - [MODEL_COMPONENTS] real value with specified exponent', &
'', &
'SYNOPSIS', &
'  result = set_exponent(x, i)', &
'', &
'           elemental real(kind=KIND) function set_exponent(x,i)', &
'', &
'            real(kind=KIND),intent(in) :: x', &
'            integer(kind=**),intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  X is type real', &
'', &
'  o  I is type integer', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  The return value is of the same type and kind as X.', &
'', &
'DESCRIPTION', &
'  SET_EXPONENT(3) returns the real number whose fractional part is that of X', &
'  and whose exponent part is I.', &
'', &
'OPTIONS', &
'  o  X : Shall be of type real.', &
'', &
'  o  I : Shall be of type integer.', &
'', &
'RESULT', &
'  The return value is of the same type and kind as X. The real number whose', &
'  fractional part is that of X and whose exponent part if I is returned; it is', &
'  FRACTION(X) * RADIX(X)**I.', &
'', &
'  If X has the value zero, the result has the same value as X.', &
'', &
'  If X is an IEEE infinity, the result is an IEEE NaN.', &
'', &
'  If X is an IEEE NaN, the result is the same NaN.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_setexp', &
'      implicit none', &
'      real :: x = 178.1387e-4', &
'      integer :: i = 17', &
'         print *, set_exponent(x, i), fraction(x) * radix(x)**i', &
'      end program demo_setexp', &
'', &
'  Results:', &
'', &
'            74716.7891       74716.7891', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SCALE(3), SPACING(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022        set_exponent(3fortran)', &
'']

shortname="set_exponent"
call process()


case('175','shape')

textblock=[character(len=256) :: &
'', &
'shape(3fortran)                                                shape(3fortran)', &
'', &
'NAME', &
'  SHAPE(3) - [ARRAY:INQUIRY] Determine the shape of an array or scalar', &
'', &
'SYNOPSIS', &
'  result = shape( source [,kind] )', &
'', &
'         integer(kind=KIND) function shape( source, KIND )', &
'', &
'          type(TYPE(kind=**)),intent(in)       :: source(..)', &
'          integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  SOURCE is an array or scalar of any type. If SOURCE is a pointer it must', &
'     be associated and allocatable arrays must be allocated. It shall not be', &
'     an assumed-size array.', &
'', &
'  o  KIND is a constant integer initialization expression.', &
'', &
'  o  the result is an integer array of rank one with size equal to the rank of', &
'     SOURCE of the kind specified by KIND if KIND is present, otherwise it has', &
'     the default integer kind.', &
'', &
'DESCRIPTION', &
'  SHAPE(3) queries the shape of an array.', &
'', &
'OPTIONS', &
'  o  SOURCE : an array or scalar of any type. If SOURCE is a pointer it must', &
'     be associated and allocatable arrays must be allocated.', &
'', &
'  o  KIND : indicates the kind parameter of the result.', &
'', &
'RESULT', &
'  An integer array of rank one with as many elements as SOURCE has dimensions.', &
'', &
'  The elements of the resulting array correspond to the extent of SOURCE along', &
'  the respective dimensions.', &
'', &
'  If SOURCE is a scalar, the result is an empty array (a rank-one array of', &
'  size zero).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_shape', &
'      implicit none', &
'      character(len=*),parameter :: all=''(*(g0,1x))''', &
'      integer, dimension(-1:1, -1:2) :: a', &
'         print all, ''shape of array='',shape(a)', &
'         print all, ''shape of constant='',shape(42)', &
'         print all, ''size of shape of constant='',size(shape(42))', &
'         print all, ''ubound of array='',ubound(a)', &
'         print all, ''lbound of array='',lbound(a)', &
'      end program demo_shape', &
'', &
'  Results:', &
'', &
'         shape of array= 3 4', &
'         shape of constant=', &
'         size of shape of constant= 0', &
'         ubound of array= 1 2', &
'         lbound of array= -1 -1', &
'', &
'STANDARD', &
'  Fortran 95 ; with KIND argument Fortran 2003', &
'', &
'SEE ALSO', &
'  Array inquiry:', &
'', &
'  o  SIZE(3) - Determine the size of an array', &
'', &
'  o  RANK(3) - Rank of a data object', &
'', &
'  o  UBOUND(3) - Upper dimension bounds of an array', &
'', &
'  o  LBOUND(3) - Lower dimension bounds of an array', &
'', &
'  State Inquiry:', &
'', &
'  o  ALLOCATED(3) - Status of an allocatable entity', &
'', &
'  o  IS_CONTIGUOUS(3) - Test if object is contiguous', &
'', &
'  Kind Inquiry:', &
'', &
'  o  KIND(3) - Kind of an entity', &
'', &
'  Bit Inquiry:', &
'', &
'  o  STORAGE_SIZE(3) - Storage size in bits', &
'', &
'  o  BIT_SIZE(3) - Bit size inquiry function', &
'', &
'  o  BTEST(3) - Tests a bit of an integer value.', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022               shape(3fortran)', &
'']

shortname="shape"
call process()


case('176','shifta')

textblock=[character(len=256) :: &
'', &
'shifta(3fortran)                                              shifta(3fortran)', &
'', &
'NAME', &
'  SHIFTA(3) - [BIT:SHIFT] Right shift with fill', &
'', &
'SYNOPSIS', &
'  result = shifta(i, shift )', &
'', &
'           elemental integer(kind=KIND) function shifta(i, shift)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: shift', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer of any kind', &
'', &
'  o  SHIFT is an integer of any kind', &
'', &
'  o  the result will automatically be of the same type, kind and rank as I.', &
'', &
'DESCRIPTION', &
'  SHIFTA(3) returns a value corresponding to I with all of the bits shifted', &
'  right by SHIFT places and the vacated bits on the left filled with the value', &
'  of the original left-most bit.', &
'', &
'OPTIONS', &
'  o  I : The initial value to shift and fill', &
'', &
'  o  SHIFT : how many bits to shift right. It shall be nonnegative and less', &
'     than or equal to BIT_SIZE(I). or the value is undefined. If SHIFT is zero', &
'     the result is I.', &
'', &
'RESULT', &
'  The result has the value obtained by shifting the bits of I to the right', &
'  SHIFT bits and replicating the leftmost bit of I in the left SHIFT bits', &
'  (Note the leftmost bit in "two''s complement" representation is the sign', &
'  bit).', &
'', &
'  Bits shifted out from the right end are lost.', &
'', &
'  If SHIFT is zero the result is I.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_shifta', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer(kind=int32) :: ival', &
'      integer             :: shift', &
'      integer(kind=int32) :: oval', &
'      integer(kind=int32),allocatable :: ivals(:)', &
'      integer             :: i', &
'      integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])', &
'', &
'        ! basic usage', &
'        write(*,*)shifta(100,3)', &
'', &
'        ! loop through some interesting values', &
'         shift=5', &
'', &
'         ivals=[ -1, -0, +0, +1, &', &
'         & int(b"01010101010101010101010101010101"), &', &
'         & int(b"10101010101010101010101010101010"), &', &
'         & int(b"00000000000000000000000000011111") ]', &
'', &
'         ! does your platform distinguish between +0 and -0?', &
'         ! note the original leftmost bit is used to fill in the vacated bits', &
'', &
'         write(*,''(/,"SHIFT =  ",i0)'') shift', &
'         do i=1,size(ivals)', &
'            ival=ivals(i)', &
'            write(*,''(  "I =      ",b32.32," == ",i0)'') ival,ival', &
'            oval=shifta(ival,shift)', &
'            write(*,''(  "RESULT = ",b32.32," == ",i0)'') oval,oval', &
'         enddo', &
'         ! elemental', &
'         write(*,*)"characteristics of the result are the same as input"', &
'         write(*,''(*(g0,1x))'') &', &
'           & "kind=",kind(shifta(arr,3)), "shape=",shape(shifta(arr,3)), &', &
'           & "size=",size(shifta(arr,3)) !, "rank=",rank(shifta(arr,3))', &
'', &
'      end program demo_shifta', &
'', &
'  Results:', &
'', &
'       >           12', &
'       >', &
'       > SHIFT =  5', &
'       > I =      11111111111111111111111111111111 == -1', &
'       > RESULT = 11111111111111111111111111111111 == -1', &
'       > I =      00000000000000000000000000000000 == 0', &
'       > RESULT = 00000000000000000000000000000000 == 0', &
'       > I =      00000000000000000000000000000000 == 0', &
'       > RESULT = 00000000000000000000000000000000 == 0', &
'       > I =      00000000000000000000000000000001 == 1', &
'       > RESULT = 00000000000000000000000000000000 == 0', &
'       > I =      01010101010101010101010101010101 == 1431655765', &
'       > RESULT = 00000010101010101010101010101010 == 44739242', &
'       > I =      10101010101010101010101010101010 == -1431655766', &
'       > RESULT = 11111101010101010101010101010101 == -44739243', &
'       > I =      00000000000000000000000000011111 == 31', &
'       > RESULT = 00000000000000000000000000000000 == 0', &
'       >  characteristics of the result are the same as input', &
'       > kind= 1 shape= 2 2 size= 4', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  SHIFTL(3), SHIFTR(3), ISHFT(3), ISHFTC(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              shifta(3fortran)', &
'']

shortname="shifta"
call process()


case('177','shiftl')

textblock=[character(len=256) :: &
'', &
'shiftl(3fortran)                                              shiftl(3fortran)', &
'', &
'NAME', &
'  SHIFTL(3) - [BIT:SHIFT] Shift bits left', &
'', &
'SYNOPSIS', &
'  result = shiftl( i, shift )', &
'', &
'           elemental integer(kind=KIND) function shiftl(i, shift)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: shift', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer of any kind', &
'', &
'  o  SHIFT is an integer of any kind', &
'', &
'  o  the result will automatically be of the same type, kind and rank as I.', &
'', &
'DESCRIPTION', &
'  SHIFTL(3) returns a value corresponding to I with all of the bits shifted', &
'  left by SHIFT places.', &
'', &
'  Bits shifted out from the left end are lost, and bits shifted in from the', &
'  right end are set to 0.', &
'', &
'  If the absolute value of SHIFT is greater than BIT_SIZE(I), the value is', &
'  undefined.', &
'', &
'  For example, for a 16-bit integer left-shifted five ...', &
'', &
'          >  |a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p| <- original 16-bit example', &
'          >  |f|g|h|i|j|k|l|m|n|o|p|           <- left-shifted five', &
'          >  |f|g|h|i|j|k|l|m|n|o|p|0|0|0|0|0| <- right-padded with zeros', &
'', &
'  Note the value of the result is the same as ISHFT (I, SHIFT).', &
'', &
'OPTIONS', &
'  o  I : The initial value to shift and fill in with zeros', &
'', &
'  o  SHIFT : how many bits to shift left. It shall be nonnegative and less', &
'     than or equal to BIT_SIZE(I).', &
'', &
'RESULT', &
'  The return value is of type integer and of the same kind as I.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_shiftl', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer             :: shift', &
'      integer(kind=int32) :: oval', &
'      integer(kind=int32) :: ival', &
'      integer(kind=int32),allocatable :: ivals(:)', &
'      integer             :: i', &
'', &
'        print *, '' basic usage''', &
'        ival=100', &
'        write(*,*)ival, shiftl(ival,3)', &
'', &
'       ! elemental (input values may be conformant arrays)', &
'        print *, '' elemental''', &
'', &
'       ! loop through some ivalues', &
'         shift=9', &
'         ivals=[ &', &
'         & int(b"01010101010101010101010101010101"), &', &
'         & int(b"10101010101010101010101010101010"), &', &
'         & int(b"11111111111111111111111111111111") ]', &
'', &
'         write(*,''(/,"SHIFT =  ",i0)'') shift', &
'         do i=1,size(ivals)', &
'            ! print initial value as binary and decimal', &
'            write(*,''(  "I =      ",b32.32," == ",i0)'') ivals(i),ivals(i)', &
'            ! print shifted value as binary and decimal', &
'            oval=shiftl(ivals(i),shift)', &
'            write(*,''(  "RESULT = ",b32.32," == ",i0)'') oval,oval', &
'         enddo', &
'', &
'        ! more about elemental', &
'         ELEM : block', &
'         integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])', &
'         write(*,*)"characteristics of the result are the same as input"', &
'         write(*,''(*(g0,1x))'') &', &
'           & "kind=",kind(shiftl(arr,3)), "shape=",shape(shiftl(arr,3)), &', &
'           & "size=",size(shiftl(arr,3)) !, "rank=",rank(shiftl(arr,3))', &
'         endblock ELEM', &
'', &
'      end program demo_shiftl', &
'', &
'  Results:', &
'', &
'       >    basic usage', &
'       >           100         800', &
'       >    elemental', &
'       >', &
'       >  SHIFT =  9', &
'       >  I =      01010101010101010101010101010101 == 1431655765', &
'       >  RESULT = 10101010101010101010101000000000 == -1431655936', &
'       >  I =      10101010101010101010101010101010 == -1431655766', &
'       >  RESULT = 01010101010101010101010000000000 == 1431655424', &
'       >  I =      11111111111111111111111111111111 == -1', &
'       >  RESULT = 11111111111111111111111000000000 == -512', &
'       >   characteristics of the result are the same as input', &
'       >  kind= 1 shape= 2 2 size= 4', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  SHIFTA(3), SHIFTR(3), ISHFT(3), ISHFTC(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              shiftl(3fortran)', &
'']

shortname="shiftl"
call process()


case('178','shiftr')

textblock=[character(len=256) :: &
'', &
'shiftr(3fortran)                                              shiftr(3fortran)', &
'', &
'NAME', &
'  SHIFTR(3) - [BIT:SHIFT] Shift bits right', &
'', &
'SYNOPSIS', &
'  result = shiftr( i, shift )', &
'', &
'           elemental integer(kind=KIND) function shiftr(i, shift)', &
'', &
'            integer(kind=KIND),intent(in) :: i', &
'            integer(kind=**),intent(in) :: shift', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  I is an integer of any kind', &
'', &
'  o  SHIFT is an integer of any kind', &
'', &
'  o  the result will automatically be of the same type, kind and rank as I.', &
'', &
'DESCRIPTION', &
'  SHIFTR(3) returns a value corresponding to I with all of the bits shifted', &
'  right by SHIFT places.', &
'', &
'  If the absolute value of SHIFT is greater than BIT_SIZE(I), the value is', &
'  undefined.', &
'', &
'  Bits shifted out from the right end are lost, and bits shifted in from the', &
'  left end are set to 0.', &
'', &
'  For example, for a 16-bit integer right-shifted five ...', &
'', &
'          >  |a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p| <- original 16-bit example', &
'          >            |a|b|c|d|e|f|g|h|i|j|k| <- right-shifted five', &
'          >  |0|0|0|0|0|f|g|h|i|j|k|l|m|n|o|p| <- left-padded with zeros', &
'', &
'  Note the value of the result is the same as ISHFT (I, -SHIFT).', &
'', &
'OPTIONS', &
'  o  I : The value to shift', &
'', &
'  o  SHIFT : How many bits to shift right. It shall be nonnegative and less', &
'     than or equal to BIT_SIZE(I).', &
'', &
'RESULT', &
'  The remaining bits shifted right SHIFT positions. Vacated positions on the', &
'  left are filled with zeros.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_shiftr', &
'      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64', &
'      implicit none', &
'      integer             :: shift', &
'      integer(kind=int32) :: oval', &
'      integer(kind=int32) :: ival', &
'      integer(kind=int32),allocatable :: ivals(:)', &
'      integer             :: i', &
'', &
'        print *,'' basic usage''', &
'        ival=100', &
'        write(*,*)ival, shiftr(100,3)', &
'', &
'        ! elemental (input values may be conformant arrays)', &
'        print *,'' elemental''', &
'         shift=9', &
'         ivals=[ &', &
'         & int(b"01010101010101010101010101010101"), &', &
'         & int(b"10101010101010101010101010101010"), &', &
'         & int(b"11111111111111111111111111111111") ]', &
'', &
'         write(*,''(/,"SHIFT =  ",i0)'') shift', &
'         do i=1,size(ivals)', &
'            ! print initial value as binary and decimal', &
'            write(*,''(  "I =      ",b32.32," == ",i0)'') ivals(i),ivals(i)', &
'            ! print shifted value as binary and decimal', &
'            oval=shiftr(ivals(i),shift)', &
'            write(*,''(  "RESULT = ",b32.32," == ",i0,/)'') oval,oval', &
'         enddo', &
'', &
'         ! more on elemental', &
'         ELEM : block', &
'         integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])', &
'         write(*,*)"characteristics of the result are the same as input"', &
'         write(*,''(*(g0,1x))'') &', &
'           & "kind=",kind(shiftr(arr,3)), "shape=",shape(shiftr(arr,3)), &', &
'           & "size=",size(shiftr(arr,3)) !, "rank=",rank(shiftr(arr,3))', &
'         endblock ELEM', &
'', &
'      end program demo_shiftr', &
'', &
'  Results:', &
'', &
'        >    basic usage', &
'        >           100          12', &
'        >    elemental', &
'        >', &
'        >  SHIFT =  9', &
'        >  I =      01010101010101010101010101010101 == 1431655765', &
'        >  RESULT = 00000000001010101010101010101010 == 2796202', &
'        >', &
'        >  I =      10101010101010101010101010101010 == -1431655766', &
'        >  RESULT = 00000000010101010101010101010101 == 5592405', &
'        >', &
'        >  I =      11111111111111111111111111111111 == -1', &
'        >  RESULT = 00000000011111111111111111111111 == 8388607', &
'        >', &
'        >   characteristics of the result are the same as input', &
'        >  kind= 1 shape= 2 2 size= 4', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  SHIFTA(3), SHIFTL(3), ISHFT(3), ISHFTC(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              shiftr(3fortran)', &
'']

shortname="shiftr"
call process()


case('179','sign')

textblock=[character(len=256) :: &
'', &
'sign(3fortran)                                                  sign(3fortran)', &
'', &
'NAME', &
'  SIGN(3) - [NUMERIC] Sign copying function', &
'', &
'SYNOPSIS', &
'  result = sign(a, b)', &
'', &
'           elemental type(TYPE(kind=KIND))function sign(a, b)', &
'', &
'            type(TYPE(kind=KIND)),intent(in) :: a, b', &
'', &
'CHARACTERISTICS', &
'  o  A shall be of type integer or real.', &
'', &
'  o  B shall be of the same type as A.', &
'', &
'  o  the characteristics of the result are the same as A.', &
'', &
'DESCRIPTION', &
'  SIGN(3) returns a value with the magnitude of a but with the sign of b.', &
'', &
'  For processors that distinguish between positive and negative zeros sign()', &
'  may be used to distinguish between real values 0.0 and -0.0. SIGN (1.0,', &
'  -0.0) will return -1.0 when a negative zero is distinguishable.', &
'', &
'OPTIONS', &
'  o  A : The value whose magnitude will be returned.', &
'', &
'  o  B : The value whose sign will be returned.', &
'', &
'RESULT', &
'  a value with the magnitude of A with the sign of B. That is,', &
'', &
'  o  If b >= 0 then the result is abs(a)', &
'', &
'  o  else if b < 0 it is -abs(a).', &
'', &
'  o  if b is real and the processor distinguishes between -0.0 and 0.0 then', &
'     the result is -abs(a)', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_sign', &
'      implicit none', &
'        ! basics', &
'         print *,  sign( -12,  1 )', &
'         print *,  sign( -12,  0 )', &
'         print *,  sign( -12, -1 )', &
'         print *,  sign(  12,  1 )', &
'         print *,  sign(  12,  0 )', &
'         print *,  sign(  12, -1 )', &
'', &
'         if(sign(1.0,-0.0)== -1.0)then', &
'            print *, ''this processor distinguishes +0 from -0''', &
'         else', &
'            print *, ''this processor does not distinguish +0 from -0''', &
'         endif', &
'', &
'         print *,  ''elemental'', sign( -12.0, [1.0, 0.0, -1.0] )', &
'', &
'      end program demo_sign', &
'', &
'  Results:', &
'', &
'                   12', &
'                   12', &
'  -12 12 12 -12 this processor does not distinguish +0 from -0', &
'', &
'    elemental', &
'      12.00000       12.00000      -12.00000', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ABS(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                sign(3fortran)', &
'']

shortname="sign"
call process()


case('180','sin')

textblock=[character(len=256) :: &
'', &
'sin(3fortran)                                                    sin(3fortran)', &
'', &
'NAME', &
'  SIN(3) - [MATHEMATICS:TRIGONOMETRIC] Sine function', &
'', &
'SYNOPSIS', &
'  result = sin(x)', &
'', &
'           elemental TYPE(kind=KIND) function sin(x)', &
'', &
'            TYPE(kind=KIND) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be any real or complex type', &
'', &
'  o  KIND may be any kind supported by the associated type of X.', &
'', &
'  o  The returned value will be of the same type and kind as the argument X.', &
'', &
'DESCRIPTION', &
'  SIN(3) computes the sine of an angle given the size of the angle in radians.', &
'', &
'  The sine of an angle in a right-angled triangle is the ratio of the length', &
'  of the side opposite the given angle divided by the length of the', &
'  hypotenuse.', &
'', &
'OPTIONS', &
'  o  X : The angle in radians to compute the sine of.', &
'', &
'RESULT', &
'  o  RESULT The return value contains the processor-dependent approximation of', &
'     the sine of X', &
'', &
'     If X is of type real, it is regarded as a value in radians.', &
'', &
'     If X is of type complex, its real part is regarded as a value in radians.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program sample_sin', &
'      implicit none', &
'      real :: x = 0.0', &
'         x = sin(x)', &
'         write(*,*)''X='',x', &
'      end program sample_sin', &
'', &
'  Results:', &
'', &
'       >  X=  0.0000000E+00', &
'', &
'  Extended Example', &
'', &
'  Haversine Formula', &
'', &
'  From the article on "Haversine formula" in Wikipedia:', &
'', &
'          The haversine formula is an equation important in navigation,', &
'          giving great-circle distances between two points on a sphere from', &
'          their longitudes and latitudes.', &
'', &
'  So to show the great-circle distance between the Nashville International', &
'  Airport (BNA) in TN, USA, and the Los Angeles International Airport (LAX) in', &
'  CA, USA you would start with their latitude and longitude, commonly given as', &
'', &
'        BNA: N 36 degrees 7.2'',   W 86 degrees 40.2''', &
'        LAX: N 33 degrees 56.4'',  W 118 degrees 24.0''', &
'', &
'  which converted to floating-point values in degrees is:', &
'', &
'             Latitude Longitude', &
'', &
'    o  BNA 36.12, -86.67', &
'', &
'    o  LAX 33.94, -118.40', &
'', &
'  And then use the haversine formula to roughly calculate the distance along', &
'  the surface of the Earth between the locations:', &
'', &
'  Sample program:', &
'', &
'      program demo_sin', &
'      implicit none', &
'      real :: d', &
'          d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX', &
'          print ''(A,F9.4,A)'', ''distance: '',d,'' km''', &
'      contains', &
'      function haversine(latA,lonA,latB,lonB) result (dist)', &
'      !', &
'      ! calculate great circle distance in kilometers', &
'      ! given latitude and longitude in degrees', &
'      !', &
'      real,intent(in) :: latA,lonA,latB,lonB', &
'      real :: a,c,dist,delta_lat,delta_lon,lat1,lat2', &
'      real,parameter :: radius = 6371 ! mean earth radius in kilometers,', &
'      ! recommended by the International Union of Geodesy and Geophysics', &
'', &
'      ! generate constant pi/180', &
'      real, parameter :: deg_to_rad = atan(1.0)/45.0', &
'         delta_lat = deg_to_rad*(latB-latA)', &
'         delta_lon = deg_to_rad*(lonB-lonA)', &
'         lat1 = deg_to_rad*(latA)', &
'         lat2 = deg_to_rad*(latB)', &
'         a = (sin(delta_lat/2))**2 + &', &
'                & cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2', &
'         c = 2*asin(sqrt(a))', &
'         dist = radius*c', &
'      end function haversine', &
'      end program demo_sin', &
'', &
'  Results:', &
'', &
'       > distance: 2886.4446 km', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  ASIN(3), COS(3), TAN(3), ACOSH(3), ACOS(3), ASINH(3), ATAN2(3), ATANH(3),', &
'  ACOSH(3), ASINH(3), ATANH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:sine and cosine', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 sin(3fortran)', &
'']

shortname="sin"
call process()


case('181','sinh')

textblock=[character(len=256) :: &
'', &
'sinh(3fortran)                                                  sinh(3fortran)', &
'', &
'NAME', &
'  SINH(3) - [MATHEMATICS:TRIGONOMETRIC] Hyperbolic sine function', &
'', &
'SYNOPSIS', &
'  result = sinh(x)', &
'', &
'           elemental TYPE(kind=KIND) function sinh(x)', &
'', &
'            TYPE(kind=KIND) :: x', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be real or complex', &
'', &
'  o  KIND may be any kind supported by the associated type.', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  SINH(3) computes the hyperbolic sine of X.', &
'', &
'  The hyperbolic sine of x is defined mathematically as:', &
'', &
'           sinh(x) = (exp(x) - exp(-x)) / 2.0', &
'', &
'OPTIONS', &
'  o  X : The value to calculate the hyperbolic sine of', &
'', &
'RESULT', &
'  The result has a value equal to a processor-dependent approximation to', &
'  sinh(X). If X is of type complex its imaginary part is regarded as a value', &
'  in radians.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_sinh', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'      & real_kinds, real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = - 1.0_real64', &
'      real(kind=real64) :: nan, inf', &
'      character(len=20) :: line', &
'', &
'        ! basics', &
'         print *, sinh(x)', &
'         print *, (exp(x)-exp(-x))/2.0', &
'', &
'        ! sinh(3) is elemental and can handle an array', &
'         print *, sinh([x,2.0*x,x/3.0])', &
'', &
'         ! a NaN input returns NaN', &
'         line=''NAN''', &
'         read(line,*) nan', &
'         print *, sinh(nan)', &
'', &
'         ! a Inf input returns Inf', &
'         line=''Infinity''', &
'         read(line,*) inf', &
'         print *, sinh(inf)', &
'', &
'         ! an overflow returns Inf', &
'         x=huge(0.0d0)', &
'         print *, sinh(x)', &
'', &
'      end program demo_sinh', &
'', &
'  Results:', &
'', &
'        -1.1752011936438014', &
'        -1.1752011936438014', &
'        -1.1752011936438014       -3.6268604078470190      -0.33954055725615012', &
'                             NaN', &
'                        Infinity', &
'                        Infinity', &
'', &
'STANDARD', &
'  Fortran 95 , for a complex argument Fortran 2008', &
'', &
'SEE ALSO', &
'  ASINH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:hyperbolic functions', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                sinh(3fortran)', &
'']

shortname="sinh"
call process()


case('182','size')

textblock=[character(len=256) :: &
'', &
'size(3fortran)                                                  size(3fortran)', &
'', &
'NAME', &
'  SIZE(3) - [ARRAY:INQUIRY] Determine the size of an array or extent of one', &
'  dimension', &
'', &
'SYNOPSIS', &
'  result = size(array [,dim] [,kind])', &
'', &
'           integer(kind=KIND) function size(array,dim,kind)', &
'', &
'            type(TYPE(kind=KIND),intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            integer(kind=**),intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY is an assumed-rank array or array of any type and associated kind.', &
'', &
'     If ARRAY is a pointer it must be associated and allocatable arrays must', &
'     be allocated.', &
'', &
'  o  DIM is an integer scalar', &
'', &
'  o  KIND is a scalar integer constant expression.', &
'', &
'  o  the result is an integer scalar of kind KIND. If KIND is absent a integer', &
'     of default kind is returned.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  SIZE(3) returns the total number of elements in an array, or if DIM is', &
'  specified returns the number of elements along that dimension.', &
'', &
'  SIZE(3) determines the extent of ARRAY along a specified dimension DIM, or', &
'  the total number of elements in ARRAY if DIM is absent.', &
'', &
'OPTIONS', &
'  o  ARRAY : the array to measure the number of elements of. If *array is an', &
'     assumed-size array, DIM shall be present with a value less than the rank', &
'     of ARRAY.', &
'', &
'  o  DIM : a value shall be in the range from 1 to n, where n equals the rank', &
'     of ARRAY.', &
'', &
'     If not present the total number of elements of the entire array are', &
'     returned.', &
'', &
'  o  KIND : An integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'     If absent the kind type parameter of the returned value is that of', &
'     default integer type.', &
'', &
'     The KIND must allow for the magnitude returned by SIZE or results are', &
'     undefined.', &
'', &
'     If KIND is absent, the return value is of default integer kind.', &
'', &
'RESULT', &
'  If DIM is not present ARRAY is assumed-rank, the result has a value equal to', &
'  PRODUCT(SHAPE(ARRAY,KIND)). Otherwise, the result has a value equal to the', &
'  total number of elements of ARRAY.', &
'', &
'  If DIM is present the number of elements along that dimension are returned,', &
'  except that if ARRAY is assumed-rank and associated with an assumed-size', &
'  array and DIM is present with a value equal to the rank of ARRAY, the value', &
'  is -1.', &
'', &
'  NOTE1', &
'', &
'  If ARRAY is assumed-rank and has rank zero, DIM cannot be present since it', &
'  cannot satisfy the requirement', &
'', &
'  1 <= DIM <= 0.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_size', &
'      implicit none', &
'      integer :: arr(0:2,-5:5)', &
'         write(*,*)''SIZE of simple two-dimensional array''', &
'         write(*,*)''SIZE(arr)       :total count of elements:'',size(arr)', &
'         write(*,*)''SIZE(arr,DIM=1) :number of rows         :'',size(arr,dim=1)', &
'         write(*,*)''SIZE(arr,DIM=2) :number of columns      :'',size(arr,dim=2)', &
'', &
'         ! pass the same array to a procedure that passes the value two', &
'         ! different ways', &
'         call interfaced(arr,arr)', &
'      contains', &
'', &
'      subroutine interfaced(arr1,arr2)', &
'      ! notice the difference in the array specification', &
'      ! for arr1 and arr2.', &
'      integer,intent(in) :: arr1(:,:)', &
'      integer,intent(in) :: arr2(2,*)', &
'         !', &
'         write(*,*)''interfaced assumed-shape array''', &
'         write(*,*)''SIZE(arr1)        :'',size(arr1)', &
'         write(*,*)''SIZE(arr1,DIM=1)  :'',size(arr1,dim=1)', &
'         write(*,*)''SIZE(arr1,DIM=2)  :'',size(arr1,dim=2)', &
'', &
'      !  write(*,*)''SIZE(arr2)        :'',size(arr2)', &
'         write(*,*)''SIZE(arr2,DIM=1)  :'',size(arr2,dim=1)', &
'      !', &
'      ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION', &
'      !  write(*,*)''SIZE(arr2,DIM=2)  :'',size(arr2,dim=2)', &
'', &
'      end subroutine interfaced', &
'', &
'      end program demo_size', &
'', &
'  Results:', &
'', &
'          SIZE of simple two-dimensional array', &
'          SIZE(arr)       :total count of elements:          33', &
'          SIZE(arr,DIM=1) :number of rows         :           3', &
'          SIZE(arr,DIM=2) :number of columns      :          11', &
'          interfaced assumed-shape array', &
'          SIZE(arr1)        :          33', &
'          SIZE(arr1,DIM=1)  :           3', &
'          SIZE(arr1,DIM=2)  :          11', &
'          SIZE(arr2,DIM=1)  :           2', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  Array inquiry:', &
'', &
'  o  SIZE(3) - Determine the size of an array', &
'', &
'  o  RANK(3) - Rank of a data object', &
'', &
'  o  SHAPE(3) - Determine the shape of an array', &
'', &
'  o  UBOUND(3) - Upper dimension bounds of an array', &
'', &
'  o  LBOUND(3) - Lower dimension bounds of an array', &
'', &
'  State Inquiry:', &
'', &
'  o  ALLOCATED(3) - Status of an allocatable entity', &
'', &
'  o  IS_CONTIGUOUS(3) - Test if object is contiguous', &
'', &
'  Kind Inquiry:', &
'', &
'  o  KIND(3) - Kind of an entity', &
'', &
'  Bit Inquiry:', &
'', &
'  o  STORAGE_SIZE(3) - Storage size in bits', &
'', &
'  o  BIT_SIZE(3) - Bit size inquiry function', &
'', &
'  o  BTEST(3) - Tests a bit of an integer value.', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                size(3fortran)', &
'']

shortname="size"
call process()


case('183','spacing')

textblock=[character(len=256) :: &
'', &
'spacing(3fortran)                                            spacing(3fortran)', &
'', &
'NAME', &
'  SPACING(3) - [MODEL_COMPONENTS] Smallest distance between two numbers of a', &
'  given type', &
'', &
'SYNOPSIS', &
'  result = spacing(x)', &
'', &
'           elemental real(kind=KIND) function spacing(x)', &
'', &
'            real(kind=KIND), intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X is type real of any valid kind', &
'', &
'  o  The result is of the same type as the input argument X.', &
'', &
'DESCRIPTION', &
'  SPACING(3) determines the distance between the argument X and the nearest', &
'  adjacent number of the same type.', &
'', &
'OPTIONS', &
'  o  X : Shall be of type real.', &
'', &
'RESULT', &
'  If X does not have the value zero and is not an IEEE infinity or NaN, the', &
'  result has the value nearest to X for values of the same type and kind', &
'  assuming the value is representable.', &
'', &
'  Otherwise, the value is the same as TINY(X). + zero produces TINY(X) + IEEE', &
'  Infinity produces an IEEE Nan + if an IEEE NaN, that NaN is returned', &
'', &
'  If there are two extended model values equally near to X, the value of', &
'  greater absolute value is taken.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_spacing', &
'      implicit none', &
'      integer, parameter :: sgl = selected_real_kind(p=6, r=37)', &
'      integer, parameter :: dbl = selected_real_kind(p=13, r=200)', &
'', &
'         write(*,*) spacing(1.0_sgl)', &
'         write(*,*) nearest(1.0_sgl,+1.0),nearest(1.0_sgl,+1.0)-1.0', &
'', &
'         write(*,*) spacing(1.0_dbl)', &
'      end program demo_spacing', &
'', &
'  Results:', &
'', &
'  Typical values ...', &
'', &
'           1.1920929E-07', &
'            1.000000      1.1920929E-07', &
'           0.9999999     -5.9604645E-08', &
'           2.220446049250313E-016', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SCALE(3), SET_EXPONENT(3), TINY(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022             spacing(3fortran)', &
'']

shortname="spacing"
call process()


case('184','spread')

textblock=[character(len=256) :: &
'', &
'spread(3fortran)                                              spread(3fortran)', &
'', &
'NAME', &
'  SPREAD(3) - [ARRAY:CONSTRUCTION] Add a dimension and replicate data', &
'', &
'SYNOPSIS', &
'  result = spread(source, dim, ncopies)', &
'', &
'           TYPE(kind=KIND) function spread(source, dim, ncopies)', &
'', &
'            TYPE(kind=KIND)             :: source(..)', &
'            integer(kind=**),intent(in) :: dim', &
'            integer(kind=**),intent(in) :: ncopies', &
'', &
'CHARACTERISTICS', &
'  o  SOURCE is a scalar or array of any type.', &
'', &
'  o  DIM is an integer scalar', &
'', &
'  o  NCOPIES is an integer scalar', &
'', &
'DESCRIPTION', &
'  SPREAD(3) replicates a SOURCE array along a specified dimension DIM. The', &
'  copy is repeated NCOPIES times.', &
'', &
'  So to add additional rows to a matrix DIM=1 would be used, but to add', &
'  additional rows DIM=2 would be used, for example.', &
'', &
'  If SOURCE is scalar, the size of the resulting vector is NCOPIES and each', &
'  element of the result has a value equal to SOURCE.', &
'', &
'OPTIONS', &
'  o  SOURCE : a scalar or array of any type and a rank less than fifteen.', &
'', &
'  o  DIM', &
'', &
'      : The additional dimension value in the range from 1 to N+1, where N', &
'      equals the rank of SOURCE.', &
'', &
'  o  NCOPIES : the number of copies of the original data to generate', &
'', &
'RESULT', &
'  The result is an array of the same type as SOURCE and has rank N+1 where N', &
'  equals the rank of SOURCE.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_spread', &
'      implicit none', &
'', &
'      integer a1(4,3), a2(3,4), v(4), s', &
'', &
'         write(*,''(a)'' ) &', &
'         ''TEST SPREAD(3)                                      '', &', &
'         ''  SPREAD(3) is a FORTRAN90 function which replicates'', &', &
'         ''  an array by adding a dimension.                   '', &', &
'         '' ''', &
'', &
'         s = 99', &
'         call printi(''suppose we have a scalar S'',s)', &
'', &
'         write(*,*) ''to add a new dimension (1) of extent 4 call''', &
'         call printi(''spread( s, dim=1, ncopies=4 )'',spread ( s, 1, 4 ))', &
'', &
'         v = [ 1, 2, 3, 4 ]', &
'         call printi('' first we will set V to'',v)', &
'', &
'         write(*,''(a)'')'' and then do "spread ( v, dim=2, ncopies=3 )"''', &
'         a1 = spread ( v, dim=2, ncopies=3 )', &
'         call printi(''this adds a new dimension (2) of extent 3'',a1)', &
'', &
'         a2 = spread ( v, 1, 3 )', &
'         call printi('' spread(v,1,3) adds a new dimension (1) of extent 3'',a2)', &
'         ! add more', &
'         a2 = spread ( v, 1, 3 )', &
'         call printi('' spread(v,1,3) adds a new dimension (1) of extent 3'',a2)', &
'', &
'      contains', &
'      ! CONVENIENCE ROUTINE; NOT DIRECTLY CONNECTED TO SPREAD(3)', &
'      subroutine printi(title,a)', &
'      use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,&', &
'       & stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT', &
'      implicit none', &
'', &
'      !@(#) print small 2d integer scalar, vector, matrix in row-column format', &
'', &
'      character(len=*),parameter   :: all=''(" ",*(g0,1x))''', &
'      character(len=*),intent(in)  :: title', &
'      character(len=20)            :: row', &
'      integer,intent(in)           :: a(..)', &
'      integer                      :: i', &
'', &
'         write(*,all,advance=''no'')trim(title)', &
'         ! select rank of input', &
'         select rank(a)', &
'         rank (0); write(*,''(a)'')'' (a scalar)''', &
'            write(*,''(" > [ ",i0," ]")'')a', &
'         rank (1); write(*,''(a)'')'' (a vector)''', &
'            ! find how many characters to use for integers', &
'            write(row,''(i0)'')ceiling(log10(real(maxval(abs(a)))))+2', &
'            ! use this format to write a row', &
'            row=''(" > [",*(i''//trim(row)//'':,","))''', &
'            do i=1,size(a)', &
'               write(*,fmt=row,advance=''no'')a(i)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'         rank (2); write(*,''(a)'')'' (a matrix) ''', &
'            ! find how many characters to use for integers', &
'            write(row,''(i0)'')ceiling(log10(real(maxval(abs(a)))))+2', &
'            ! use this format to write a row', &
'            row=''(" > [",*(i''//trim(row)//'':,","))''', &
'            do i=1,size(a,dim=1)', &
'               write(*,fmt=row,advance=''no'')a(i,:)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'         rank default', &
'            write(stderr,*)''*printi* did not expect rank='', rank(a), &', &
'             & ''shape='', shape(a),''size='',size(a)', &
'            stop ''*printi* unexpected rank''', &
'         end select', &
'         write(*,all) ''>shape='',shape(a),'',rank='',rank(a),'',size='',size(a)', &
'         write(*,*)', &
'', &
'      end subroutine printi', &
'', &
'      end program demo_spread', &
'', &
'  Results:', &
'', &
'         TEST SPREAD(3)', &
'           SPREAD(3) is a FORTRAN90 function which replicates', &
'           an array by adding a dimension.', &
'', &
'          suppose we have a scalar S  (a scalar)', &
'          > [ 99 ]', &
'          >shape= ,rank= 0 ,size= 1', &
'', &
'          to add a new dimension (1) of extent 4 call', &
'          spread( s, dim=1, ncopies=4 )  (a vector)', &
'          > [  99 ]', &
'          > [  99 ]', &
'          > [  99 ]', &
'          > [  99 ]', &
'          >shape= 4 ,rank= 1 ,size= 4', &
'', &
'           first we will set V to  (a vector)', &
'          > [  1 ]', &
'          > [  2 ]', &
'          > [  3 ]', &
'          > [  4 ]', &
'          >shape= 4 ,rank= 1 ,size= 4', &
'', &
'          and then do "spread ( v, dim=2, ncopies=3 )"', &
'          this adds a new dimension (2) of extent 3  (a matrix)', &
'          > [  1,  1,  1 ]', &
'          > [  2,  2,  2 ]', &
'          > [  3,  3,  3 ]', &
'          > [  4,  4,  4 ]', &
'          >shape= 4 3 ,rank= 2 ,size= 12', &
'', &
'           spread(v,dim=1,ncopies=3) adds a new dimension (1) (a matrix)', &
'          > [  1,  2,  3,  4 ]', &
'          > [  1,  2,  3,  4 ]', &
'          > [  1,  2,  3,  4 ]', &
'          >shape= 3 4 ,rank= 2 ,size= 12', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  MERGE(3), PACK(3), UNPACK(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              spread(3fortran)', &
'']

shortname="spread"
call process()


case('185','sqrt')

textblock=[character(len=256) :: &
'', &
'sqrt(3fortran)                                                  sqrt(3fortran)', &
'', &
'NAME', &
'  SQRT(3) - [MATHEMATICS] Square-root function', &
'', &
'SYNOPSIS', &
'  result = sqrt(x)', &
'', &
'           elemental TYPE(kind=KIND) function sqrt(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  TYPE may be real or complex.', &
'', &
'  o  KIND may be any kind valid for the declared type.', &
'', &
'  o  the result has the same characteristics as X.', &
'', &
'DESCRIPTION', &
'  SQRT(3) computes the principal square root of X.', &
'', &
'  The number whose square root is being considered is known as the radicand.', &
'', &
'  In mathematics, a square root of a radicand X is a number Y such that Y*Y =', &
'  X.', &
'', &
'  Every nonnegative radicand X has two square roots of the same unique', &
'  magnitude, one positive and one negative. The nonnegative square root is', &
'  called the principal square root.', &
'', &
'  The principal square root of 9 is 3, for example, even though (-3)*(-3) is', &
'  also 9.', &
'', &
'  Square roots of negative numbers are a special case of complex numbers,', &
'  where with COMPLEX input the components of the radicand need not be positive', &
'  in order to have a valid square root.', &
'', &
'OPTIONS', &
'  o  X : The radicand to find the principal square root of. If X is real its', &
'     value must be greater than or equal to zero.', &
'', &
'RESULT', &
'  The principal square root of X is returned.', &
'', &
'  For a complex result the real part is greater than or equal to zero.', &
'', &
'  When the real part of the result is zero, the imaginary part has the same', &
'  sign as the imaginary part of X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_sqrt', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'       & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x, x2', &
'      complex :: z, z2', &
'', &
'        ! basics', &
'         x = 2.0_real64', &
'         ! complex', &
'         z = (1.0, 2.0)', &
'         write(*,*)''input values '',x,z', &
'', &
'         x2 = sqrt(x)', &
'         z2 = sqrt(z)', &
'         write(*,*)''output values '',x2,z2', &
'', &
'        ! elemental', &
'        write(*,*)''elemental'',sqrt([64.0,121.0,30.0])', &
'', &
'        ! alternatives', &
'         x2 = x**0.5', &
'         z2 = z**0.5', &
'         write(*,*)''alternatively'',x2,z2', &
'', &
'      end program demo_sqrt', &
'', &
'  Results:', &
'', &
'          input values    2.00000000000000      (1.000000,2.000000)', &
'          output values    1.41421356237310      (1.272020,0.7861513)', &
'          elemental   8.000000       11.00000       5.477226', &
'          alternatively   1.41421356237310      (1.272020,0.7861513)', &
'', &
'STANDARD', &
'  FORTRAN 77', &
'', &
'SEE ALSO', &
'  EXP(3), LOG(3), LOG10(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                sqrt(3fortran)', &
'']

shortname="sqrt"
call process()


case('186','storage_size')

textblock=[character(len=256) :: &
'', &
'storage_size(3fortran)                                  storage_size(3fortran)', &
'', &
'NAME', &
'  STORAGE_SIZE(3) - [BIT:INQUIRY] Storage size in bits', &
'', &
'SYNOPSIS', &
'  result = storage_size(a [,KIND] )', &
'', &
'           integer(kind=KIND) storage_size(a,KIND)', &
'', &
'            type(TYPE(kind=**)) :: a', &
'            integer,intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  A may be of any type and kind. If it is polymorphic it shall not be an', &
'     undefined pointer. If it is unlimited polymorphic or has any deferred', &
'     type parameters, it shall not be an unallocated allocatable variable or a', &
'     disassociated or undefined pointer.', &
'', &
'  o  The kind type parameter of the returned value is that specified by the', &
'     value of KIND; otherwise, the kind type parameter is that of default', &
'     integer type.', &
'', &
'  o  The result is an integer scalar of default kind unless KIND is specified,', &
'     in which case it has the kind specified by KIND.', &
'', &
'DESCRIPTION', &
'  STORAGE_SIZE(3) returns the storage size of argument A in bits.', &
'', &
'OPTIONS', &
'  o  A : The entity to determine the storage size of', &
'', &
'  o  KIND : a scalar integer constant expression that defines the kind of the', &
'     output value.', &
'', &
'RESULT', &
'  The result value is the size expressed in bits for an element of an array', &
'  that has the dynamic type and type parameters of A.', &
'', &
'  If the type and type parameters are such that storage association applies,', &
'  the result is consistent with the named constants defined in the intrinsic', &
'  module ISO_FORTRAN_ENV.', &
'', &
'  NOTE1', &
'', &
'  An array element might take "type" more bits to store than an isolated', &
'  scalar, since any hardware-imposed alignment requirements for array elements', &
'  might not apply to a simple scalar variable.', &
'', &
'  NOTE2', &
'', &
'  This is intended to be the size in memory that an object takes when it is', &
'  stored; this might differ from the size it takes during expression handling', &
'  (which might be the native register size) or when stored in a file. If an', &
'  object is never stored in memory but only in a register, this function', &
'  nonetheless returns the size it would take if it were stored in memory.', &
'', &
'EXAMPLES', &
'  Sample program', &
'', &
'      program demo_storage_size', &
'      implicit none', &
'', &
'         ! a default real, integer, and logical are the same storage size', &
'         write(*,*)''size of integer       '',storage_size(0)', &
'         write(*,*)''size of real          '',storage_size(0.0)', &
'         write(*,*)''size of logical       '',storage_size(.true.)', &
'         write(*,*)''size of complex       '',storage_size((0.0,0.0))', &
'', &
'         ! note the size of an element of the array, not the storage size of', &
'         ! the entire array is returned for array arguments', &
'         write(*,*)''size of integer array '',storage_size([0,1,2,3,4,5,6,7,8,9])', &
'', &
'      end program demo_storage_size', &
'', &
'  Results:', &
'', &
'          size of integer                 32', &
'          size of real                    32', &
'          size of logical                 32', &
'          size of complex                 64', &
'          size of integer array           32', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  C_SIZEOF(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022        storage_size(3fortran)', &
'']

shortname="storage_size"
call process()


case('187','sum')

textblock=[character(len=256) :: &
'', &
'sum(3fortran)                                                    sum(3fortran)', &
'', &
'NAME', &
'  SUM(3) - [ARRAY:REDUCTION] Sum the elements of an array', &
'', &
'SYNOPSIS', &
'  result = sum(array [,dim[,mask]] | [mask] )', &
'', &
'           TYPE(kind=KIND) function sum(array, dim, mask)', &
'', &
'            TYPE(kind=KIND),intent(in) :: array(..)', &
'            integer(kind=**),intent(in),optional :: dim', &
'            logical(kind=**),intent(in),optional :: mask(..)', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  ARRAY may be of any numeric type - integer, real or complex.', &
'', &
'  o  DIM is an integer', &
'', &
'  o  MASK is logical and conformable with ARRAY.', &
'', &
'  o  The result is of the same type and kind as ARRAY. It is scalar if DIM is', &
'     not present or ARRAY is a vector, else it is an array.', &
'', &
'DESCRIPTION', &
'  SUM(3) adds the elements of ARRAY.', &
'', &
'  When only ARRAY is specified all elements are summed, but groups of sums may', &
'  be returned along the dimension specified by DIM and/or elements to add may', &
'  be selected by a logical mask.', &
'', &
'  No method is designated for how the sum is conducted, so whether or not', &
'  accumulated error is compensated for is processor-dependent.', &
'', &
'OPTIONS', &
'  o  ARRAY : an array containing the elements to add', &
'', &
'  o  DIM : a value in the range from 1 to n, where n equals the rank (the', &
'     number of dimensions) of ARRAY. DIM designates the dimension along which', &
'     to create sums. When absent a scalar sum of the elements optionally', &
'     selected by MASK is returned.', &
'', &
'  o  MASK : an array of the same shape as ARRAY that designates which elements', &
'     to add. If absent all elements are used in the sum(s).', &
'', &
'RESULT', &
'  If DIM is absent, a scalar with the sum of all selected elements in ARRAY is', &
'  returned. Otherwise, an array of rank n-1, where n equals the rank of ARRAY,', &
'  and a shape similar to that of ARRAY with dimension DIM dropped is returned.', &
'  Since a vector has a rank of one, the result is a scalar (if n==1, n-1 is', &
'  zero; and a rank of zero means a scalar).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_sum', &
'      implicit none', &
'      integer :: vector(5) , matrix(3,4), box(5,6,7)', &
'', &
'         vector = [ 1, 2, -3, 4, 5 ]', &
'', &
'         matrix(1,:)=[  -1,   2,    -3,   4    ]', &
'         matrix(2,:)=[  10,   -20,  30,   -40  ]', &
'         matrix(3,:)=[  100,  200, -300,  400  ]', &
'', &
'         box=11', &
'', &
'        ! basics', &
'         print *, ''sum all elements:'',sum(vector)', &
'         print *, ''real :'',sum([11.0,-5.0,20.0])', &
'         print *, ''complex :'',sum([(1.1,-3.3),(4.0,5.0),(8.0,-6.0)])', &
'        ! with MASK option', &
'         print *, ''sum odd elements:'',sum(vector, mask=mod(vector, 2)==1)', &
'         print *, ''sum positive values:'', sum(vector, mask=vector>0)', &
'', &
'         call printi(''the input array'', matrix )', &
'         call printi(''sum of all elements in matrix'', sum(matrix) )', &
'         call printi(''sum of positive elements'', sum(matrix,matrix>=0) )', &
'        ! along dimensions', &
'         call printi(''sum along rows'', sum(matrix,dim=1) )', &
'         call printi(''sum along columns'', sum(matrix,dim=2) )', &
'         call printi(''sum of a vector is always a scalar'', sum(vector,dim=1) )', &
'         call printi(''sum of a volume by row'', sum(box,dim=1) )', &
'         call printi(''sum of a volume by column'', sum(box,dim=2) )', &
'         call printi(''sum of a volume by depth'', sum(box,dim=3) )', &
'', &
'      contains', &
'      ! CONVENIENCE ROUTINE; NOT DIRECTLY CONNECTED TO SPREAD(3)', &
'      subroutine printi(title,a)', &
'      use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,&', &
'       & stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT', &
'      implicit none', &
'', &
'      !@(#) print small 2d integer scalar, vector, matrix in row-column format', &
'', &
'      character(len=*),intent(in)  :: title', &
'      integer,intent(in)           :: a(..)', &
'', &
'      character(len=*),parameter   :: all=''(" ",*(g0,1x))''', &
'      character(len=20)            :: row', &
'      integer,allocatable          :: b(:,:)', &
'      integer                      :: i', &
'         write(*,all,advance=''no'')trim(title)', &
'         ! copy everything to a matrix to keep code simple', &
'         select rank(a)', &
'         rank (0); write(*,''(a)'')'' (a scalar)''; b=reshape([a],[1,1])', &
'         rank (1); write(*,''(a)'')'' (a vector)''; b=reshape(a,[size(a),1])', &
'         rank (2); write(*,''(a)'')'' (a matrix)''; b=a', &
'         rank default; stop ''*printi* unexpected rank''', &
'         end select', &
'         ! find how many characters to use for integers', &
'         write(row,''(i0)'')ceiling(log10(real(maxval(abs(b)))))+2', &
'         ! use this format to write a row', &
'         row=''(" > [",*(i''//trim(row)//'':,","))''', &
'         do i=1,size(b,dim=1)', &
'            write(*,fmt=row,advance=''no'')b(i,:)', &
'            write(*,''(" ]")'')', &
'         enddo', &
'         write(*,all) ''>shape='',shape(a),'',rank='',rank(a),'',size='',size(a)', &
'         write(*,*)', &
'      end subroutine printi', &
'      end program demo_sum', &
'', &
'  Results:', &
'', &
'          sum all elements:           9', &
'          real :   26.00000', &
'          complex : (13.10000,-4.300000)', &
'          sum odd elements:           6', &
'          sum positive values:          12', &
'          the input array  (a matrix)', &
'          > [   -1,    2,   -3,    4 ]', &
'          > [   10,  -20,   30,  -40 ]', &
'          > [  100,  200, -300,  400 ]', &
'          >shape= 3 4 ,rank= 2 ,size= 12', &
'', &
'          sum of all elements in matrix  (a scalar)', &
'          > [  382 ]', &
'          >shape= ,rank= 0 ,size= 1', &
'', &
'          sum of positive elements  (a scalar)', &
'          > [  746 ]', &
'          >shape= ,rank= 0 ,size= 1', &
'', &
'          sum along rows  (a vector)', &
'          > [  109 ]', &
'          > [  182 ]', &
'          > [ -273 ]', &
'          > [  364 ]', &
'          >shape= 4 ,rank= 1 ,size= 4', &
'', &
'          sum along columns  (a vector)', &
'          > [    2 ]', &
'          > [  -20 ]', &
'          > [  400 ]', &
'          >shape= 3 ,rank= 1 ,size= 3', &
'', &
'          sum of a vector is always a scalar  (a scalar)', &
'          > [  9 ]', &
'          >shape= ,rank= 0 ,size= 1', &
'', &
'          sum of a volume by row  (a matrix)', &
'          > [  55,  55,  55,  55,  55,  55,  55 ]', &
'          > [  55,  55,  55,  55,  55,  55,  55 ]', &
'          > [  55,  55,  55,  55,  55,  55,  55 ]', &
'          > [  55,  55,  55,  55,  55,  55,  55 ]', &
'          > [  55,  55,  55,  55,  55,  55,  55 ]', &
'          > [  55,  55,  55,  55,  55,  55,  55 ]', &
'          >shape= 6 7 ,rank= 2 ,size= 42', &
'', &
'          sum of a volume by column  (a matrix)', &
'          > [  66,  66,  66,  66,  66,  66,  66 ]', &
'          > [  66,  66,  66,  66,  66,  66,  66 ]', &
'          > [  66,  66,  66,  66,  66,  66,  66 ]', &
'          > [  66,  66,  66,  66,  66,  66,  66 ]', &
'          > [  66,  66,  66,  66,  66,  66,  66 ]', &
'          >shape= 5 7 ,rank= 2 ,size= 35', &
'', &
'          sum of a volume by depth  (a matrix)', &
'          > [  77,  77,  77,  77,  77,  77 ]', &
'          > [  77,  77,  77,  77,  77,  77 ]', &
'          > [  77,  77,  77,  77,  77,  77 ]', &
'          > [  77,  77,  77,  77,  77,  77 ]', &
'          > [  77,  77,  77,  77,  77,  77 ]', &
'          >shape= 5 6 ,rank= 2 ,size= 30', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  ALL(3) - Determines if all the values are true', &
'', &
'  o  ANY(3) - Determines if any of the values in the logical array are true.', &
'', &
'  o  COUNT(3) - Count true values in an array', &
'', &
'  o  MAXVAL(3) - Determines the maximum value in an array', &
'', &
'  o  MINVAL(3) - Minimum value of an array', &
'', &
'  o  PRODUCT(3) - Product of array elements', &
'', &
'  o  MERGE(3) - Merge variables', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 sum(3fortran)', &
'']

shortname="sum"
call process()


case('188','system_clock')

textblock=[character(len=256) :: &
'', &
'system_clock(3fortran)                                  system_clock(3fortran)', &
'', &
'NAME', &
'  SYSTEM_CLOCK(3) - [SYSTEM:TIME] Query system clock', &
'', &
'SYNOPSIS', &
'  call system_clock([count] [,count_rate] [,count_max] )', &
'', &
'           subroutine system_clock(count, count_rate, count_max)', &
'', &
'            integer,intent(out),optional  :: count', &
'            type(TYPE(kind=KIND),intent(out),optional  :: count_rate', &
'            integer,intent(out),optional  :: count_max', &
'', &
'CHARACTERISTICS', &
'  o  COUNT is an integer scalar', &
'', &
'  o  COUNT_RATE an integer or real scalar', &
'', &
'  o  COUNT_MAX an integer scalar', &
'', &
'DESCRIPTION', &
'  SYSTEM_CLOCK(3) lets you measure durations of time with the precision of the', &
'  smallest time increment generally available on a system by returning', &
'  processor-dependent values based on the current value of the processor', &
'  clock.', &
'', &
'  SYSTEM_CLOCK is typically used to measure short time intervals (system', &
'  clocks may be 24-hour clocks or measure processor clock ticks since boot,', &
'  for example). It is most often used for measuring or tracking the time spent', &
'  in code blocks in lieu of using profiling tools.', &
'', &
'  COUNT_RATE and COUNT_MAX are assumed constant (even though CPU rates can', &
'  vary on a single platform).', &
'', &
'  The accuracy of the measurements may depend on the kind of the arguments!', &
'', &
'  Whether an image has no clock, has a single clock of its own, or shares a', &
'  clock with another image, is processor dependent.', &
'', &
'  If there is no clock, or querying the clock fails, COUNT is set to', &
'  -HUGE(COUNT), and COUNT_RATE and COUNT_MAX are set to zero.', &
'', &
'OPTIONS', &
'  o  COUNT', &
'', &
'      If there is no clock, COUNT is returned as the negative value', &
'      -HUGE(COUNT).', &
'', &
'      Otherwise, the CLOCK value is incremented by one for each clock', &
'      count until the value COUNT_MAX is reached and is then reset to zero', &
'      at the next count. CLOCK therefore is a modulo value that lies in', &
'      the range 0 TO COUNT_MAX.', &
'', &
'  o  COUNT_RATE : is assigned a processor-dependent approximation to the', &
'     number of processor clock counts per second, or zero if there is no', &
'     clock. COUNT_RATE is system dependent and can vary depending on the kind', &
'     of the arguments. Generally, a large real may generate a more precise', &
'     interval.', &
'', &
'  o  COUNT_MAX : is assigned the maximum value that COUNT can have, or zero if', &
'     there is no clock.', &
'', &
'EXAMPLES', &
'  If the processor clock is a 24-hour clock that registers time at', &
'  approximately 18.20648193 ticks per second, at 11:30 A.M. the reference', &
'', &
'            call system_clock (count = c, count_rate = r, count_max = m)', &
'', &
'  defines', &
'', &
'            C = (11*3600+30*60)*18.20648193 = 753748,', &
'            R = 18.20648193, and', &
'            M = 24*3600*18.20648193-1 = 1573039.', &
'', &
'  Sample program:', &
'', &
'      program demo_system_clock', &
'      use, intrinsic :: iso_fortran_env, only : wp=>real64,int32,int64', &
'      implicit none', &
'      character(len=*),parameter :: g=''(1x,*(g0,1x))''', &
'      integer(kind=int64) :: count64, count_rate64, count_max64', &
'      integer(kind=int64) :: start64, finish64', &
'      integer(kind=int32) :: count32, count_rate32, count_max32', &
'      integer(kind=int32) :: start32, finish32', &
'      real(kind=wp)       :: time_read', &
'      real(kind=wp)       :: sum', &
'      integer             :: i', &
'', &
'        print g,''accuracy may vary with argument type!''', &
'        call system_clock(count_rate=count_rate64)', &
'        print g,''COUNT RATE FOR INT64:'',count_rate64', &
'        call system_clock(count_rate=count_rate32)', &
'        print g,''COUNT RATE FOR INT32:'',count_rate32', &
'', &
'        print g,''query all arguments''', &
'        call system_clock(count64, count_rate64, count_max64)', &
'        print g, ''COUNT_MAX='',count_max64', &
'        print g, ''COUNT_RATE='',count_rate64', &
'        print g, ''CURRENT COUNT='',count64', &
'', &
'        print g,''time some computation''', &
'        call system_clock(start64)', &
'', &
'        ! some code to time', &
'        sum=0.0_wp', &
'        do i=-huge(0)-1, huge(0)-1, 10', &
'           sum=sum+sqrt(real(i))', &
'        enddo', &
'        print g,''SUM='',sum', &
'', &
'        call system_clock(finish64)', &
'', &
'        time_read=(finish64-start64)/real(count_rate64,wp)', &
'        write(*,''(a30,1x,f7.4,1x,a)'') ''time : '', time_read, '' seconds''', &
'', &
'      end program demo_system_clock', &
'', &
'  Results:', &
'', &
'       accuracy may vary with argument type!', &
'       COUNT RATE FOR INT64: 1000000000', &
'       COUNT RATE FOR INT32: 1000', &
'       query all arguments', &
'       COUNT_MAX= 9223372036854775807', &
'       COUNT_RATE= 1000000000', &
'       CURRENT COUNT= 518240530647469', &
'       time some computation', &
'       SUM= NaN', &
'                             time :   1.6686  seconds', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DATE_AND_TIME(3), CPU_TIME(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022        system_clock(3fortran)', &
'']

shortname="system_clock"
call process()


case('189','tan')

textblock=[character(len=256) :: &
'', &
'tan(3fortran)                                                    tan(3fortran)', &
'', &
'NAME', &
'  TAN(3) - [MATHEMATICS:TRIGONOMETRIC] Tangent function', &
'', &
'SYNOPSIS', &
'  result = tan(x)', &
'', &
'       elemental TYPE(kind=KIND) function tan(x)', &
'', &
'        TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  the TYPE of X may be real or complex of any supported kind', &
'', &
'  o  The returned value will be of the same type and kind as the argument X.', &
'', &
'DESCRIPTION', &
'  TAN(3) computes the tangent of X.', &
'', &
'OPTIONS', &
'  o  X : The angle in radians to compute the tangent of for real input.  If X', &
'     is of type complex, its real part is regarded as a value in radians.', &
'', &
'RESULT', &
'  The return value is the tangent of the value X.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_tan', &
'      use, intrinsic :: iso_fortran_env, only : real_kinds, &', &
'      & real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 0.165_real64', &
'           write(*,*)x, tan(x)', &
'      end program demo_tan', &
'', &
'  Results:', &
'', &
'           0.16500000000000001       0.16651386310913616', &
'', &
'STANDARD', &
'  FORTRAN 77 . For a complex argument, Fortran 2008 .', &
'', &
'SEE ALSO', &
'  ATAN(3), ATAN2(3), COS(3), SIN(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                 tan(3fortran)', &
'']

shortname="tan"
call process()


case('190','tanh')

textblock=[character(len=256) :: &
'', &
'tanh(3fortran)                                                  tanh(3fortran)', &
'', &
'NAME', &
'  TANH(3) - [MATHEMATICS:TRIGONOMETRIC] Hyperbolic tangent function', &
'', &
'SYNOPSIS', &
'  result = tanh(x)', &
'', &
'           elemental TYPE(kind=KIND) function tanh(x)', &
'', &
'            TYPE(kind=KIND),intent(in) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be real or complex and any associated kind supported by the', &
'     processor.', &
'', &
'  o  The returned value will be of the same type and kind as the argument.', &
'', &
'DESCRIPTION', &
'  TANH(3) computes the hyperbolic tangent of X.', &
'', &
'OPTIONS', &
'  o  X : The value to compute the Hyperbolic tangent of.', &
'', &
'RESULT', &
'  Returns the hyperbolic tangent of X.', &
'', &
'  If X is complex, the imaginary part of the result is regarded as a radian', &
'  value.', &
'', &
'  If X is real, the return value lies in the range', &
'', &
'            -1 <= tanh(x) <= 1.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_tanh', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'      & real_kinds, real32, real64, real128', &
'      implicit none', &
'      real(kind=real64) :: x = 2.1_real64', &
'         write(*,*)x, tanh(x)', &
'      end program demo_tanh', &
'', &
'  Results:', &
'', &
'            2.1000000000000001       0.97045193661345386', &
'', &
'STANDARD', &
'  FORTRAN 77 , for a complex argument Fortran 2008', &
'', &
'SEE ALSO', &
'  ATANH(3)', &
'', &
'RESOURCES', &
'  o  Wikipedia:hyperbolic functions', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022                tanh(3fortran)', &
'']

shortname="tanh"
call process()


case('191','this_image')

textblock=[character(len=256) :: &
'', &
'this_image(3fortran)                                      this_image(3fortran)', &
'', &
'NAME', &
'  THIS_IMAGE(3) - [COLLECTIVE] Cosubscript index of this image', &
'', &
'SYNOPSIS', &
'  result = this_image() | = this_image(distance) | = this_image(coarray,dim)', &
'', &
'         integer function this_image( distance ,coarray, dim )', &
'', &
'          type(TYPE(kind=**),optional :: coarray[*]', &
'          integer,intent(in),optional :: distance', &
'          integer,intent(in),optional :: dim', &
'', &
'CHARACTERISTICS', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'  o  COARRAY can be of any type. If DIM is present it is required.', &
'', &
'  o  DISTANCE is not permitted together with COARRAY', &
'', &
'  o  if DIM if present, coarray is required.', &
'', &
'DESCRIPTION', &
'  THIS_IMAGE(3) returns the cosubscript for this image.', &
'', &
'OPTIONS', &
'  o  DISTANCE : Nonnegative scalar integer (not permitted together with', &
'     COARRAY).', &
'', &
'  o  COARRAY : if DIM present, required).', &
'', &
'  o  DIM : If present, DIM shall be between one and the corank of COARRAY.', &
'', &
'RESULT', &
'  Default integer. If COARRAY is not present, it is scalar; if DISTANCE is not', &
'  present or has value 0, its value is the image index on the invoking image', &
'  for the current team, for values smaller or equal distance to the initial', &
'  team, it returns the image index on the ancestor team which has a distance', &
'  of DISTANCE from the invoking team. If DISTANCE is larger than the distance', &
'  to the initial team, the image index of the initial team is returned.', &
'  Otherwise when the COARRAY is present, if DIM is not present, a rank-1 array', &
'  with corank elements is returned, containing the cosubscripts for COARRAY', &
'  specifying the invoking image. If DIM is present, a scalar is returned, with', &
'  the value of the DIM element of THIS_IMAGE(COARRAY).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_this_image', &
'      implicit none', &
'      integer :: value[*]', &
'      integer :: i', &
'         value = this_image()', &
'         sync all', &
'         if (this_image() == 1) then', &
'            do i = 1, num_images()', &
'               write(*,''(2(a,i0))'') ''value['', i, ''] is '', value[i]', &
'            end do', &
'         endif', &
'      end program demo_this_image', &
'', &
'  Results:', &
'', &
'         value[1] is 1', &
'', &
'STANDARD', &
'  Fortran 2008. With DISTANCE argument, TS 18508', &
'', &
'SEE ALSO', &
'  NUM_IMAGES(3), IMAGE_INDEX(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022          this_image(3fortran)', &
'']

shortname="this_image"
call process()


case('192','tiny')

textblock=[character(len=256) :: &
'', &
'tiny(3fortran)                                                  tiny(3fortran)', &
'', &
'NAME', &
'  TINY(3) - [NUMERIC MODEL] Smallest positive number of a real kind', &
'', &
'SYNOPSIS', &
'  result = tiny(x)', &
'', &
'           real(kind=KIND) function tiny(x)', &
'', &
'            real(kind=KIND) :: x', &
'', &
'CHARACTERISTICS', &
'  o  X may be any real scalar or array', &
'', &
'  o  the result has the same type and kind as X', &
'', &
'DESCRIPTION', &
'  TINY(3) returns the smallest positive (non zero) number of the type and kind', &
'  of X.', &
'', &
'  For real X', &
'', &
'         result = 2.0**(minexponent(x)-1)', &
'', &
'OPTIONS', &
'  o  X : The value whose kind is used to determine the model type to query', &
'', &
'RESULT', &
'  The smallest positive value for the real type of the specified kind.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_tiny', &
'      implicit none', &
'         print *, ''default real is from'', tiny(0.0), ''to'',huge(0.0)', &
'         print *, ''doubleprecision is from '', tiny(0.0d0), ''to'',huge(0.0d0)', &
'      end program demo_tiny', &
'', &
'  Results:', &
'', &
'       default real is from 1.17549435E-38 to 3.40282347E+38', &
'       doubleprecision is from 2.2250738585072014E-308 to', &
'       1.7976931348623157E+308', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  DIGITS(3), EPSILON(3), EXPONENT(3), FRACTION(3), HUGE(3), MAXEXPONENT(3),', &
'  MINEXPONENT(3), NEAREST(3), PRECISION(3), RADIX(3), RANGE(3), RRSPACING(3),', &
'  SCALE(3), SET_EXPONENT(3), SPACING(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                tiny(3fortran)', &
'']

shortname="tiny"
call process()


case('193','trailz')

textblock=[character(len=256) :: &
'', &
'trailz(3fortran)                                              trailz(3fortran)', &
'', &
'NAME', &
'  TRAILZ(3) - [BIT:COUNT] Number of trailing zero bits of an integer', &
'', &
'SYNOPSIS', &
'  result = trailz(i)', &
'', &
'        elemental integer function trailz(i)', &
'', &
'         integer(kind=**),intent(in) :: i', &
'', &
'CHARACTERISTICS', &
'  o  I is an integer of any kind.', &
'', &
'  o  the result is an integer of default kind', &
'', &
'DESCRIPTION', &
'  TRAILZ(3) returns the number of trailing zero bits of an integer value.', &
'', &
'OPTIONS', &
'  o  I : the value to count trailing zero bits in', &
'', &
'RESULT', &
'  The number of trailing rightmost zero bits in an integer value after the', &
'  last non-zero bit.', &
'', &
'             >      right-most non-zero bit', &
'             >                 V', &
'             >  |0|0|0|1|1|1|0|1|0|0|0|0|0|0|', &
'             >  ^               |___________| trailing zero bits', &
'             >   bit_size(i)', &
'', &
'  If all the bits of I are zero, the result is the size of the input value in', &
'  bits, ie. BIT_SIZE(I).', &
'', &
'  The result may also be seen as the position of the rightmost 1 bit in I,', &
'  starting with the rightmost bit being zero and counting to the left.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_trailz', &
'', &
'      ! some common integer kinds', &
'      use, intrinsic :: iso_fortran_env, only : &', &
'       & integer_kinds, int8, int16, int32, int64', &
'', &
'      implicit none', &
'', &
'      ! a handy format', &
'      character(len=*),parameter :: &', &
'       & show = ''(1x,"value=",i4,", value(bits)=",b32.32,1x,", trailz=",i3)''', &
'', &
'      integer(kind=int64) :: bigi', &
'        ! basics', &
'         write(*,*)''Note default integer is'',bit_size(0),''bits''', &
'         print  show,  -1, -1,  trailz(-1)', &
'         print  show,   0,  0,  trailz(0)', &
'         print  show,   1,  1,  trailz(1)', &
'         print  show,  96, 96,  trailz(96)', &
'        ! elemental', &
'         print *, ''elemental and any integer kind:''', &
'         bigi=2**5', &
'         write(*,*) trailz( [ bigi, bigi*256, bigi/2 ] )', &
'         write(*,''(1x,b64.64)'')[ bigi, bigi*256, bigi/2 ]', &
'', &
'      end program demo_trailz', &
'', &
'  Results:', &
'', &
'          Note default integer is          32 bits', &
'          value=  -1, value(bits)=11111111111111111111111111111111 , trailz=  0', &
'          value=   0, value(bits)=00000000000000000000000000000000 , trailz= 32', &
'          value=   1, value(bits)=00000000000000000000000000000001 , trailz=  0', &
'          value=  96, value(bits)=00000000000000000000000001100000 , trailz=  5', &
'          elemental and any integer kind:', &
'                    5          13           4', &
'          0000000000000000000000000000000000000000000000000000000000100000', &
'          0000000000000000000000000000000000000000000000000010000000000000', &
'          0000000000000000000000000000000000000000000000000000000000010000', &
'', &
'STANDARD', &
'  Fortran 2008', &
'', &
'SEE ALSO', &
'  BIT_SIZE(3), POPCNT(3), POPPAR(3), LEADZ(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              trailz(3fortran)', &
'']

shortname="trailz"
call process()


case('194','transfer')

textblock=[character(len=256) :: &
'', &
'transfer(3fortran)                                          transfer(3fortran)', &
'', &
'NAME', &
'  TRANSFER(3) - [TYPE:MOLD] Transfer bit patterns', &
'', &
'SYNOPSIS', &
'  result = transfer(source, mold [,size] )', &
'', &
'           type(TYPE(kind=KIND)) function transfer(source,mold,size)', &
'', &
'            type(TYPE(kind=KIND)),intent(in) :: source(..)', &
'            type(TYPE(kind=KIND)),intent(in) :: mold(..)', &
'            integer(kind=**),intent(in),optional :: size', &
'', &
'CHARACTERISTICS', &
'  o  SOURCE shall be a scalar or an array of any type.', &
'', &
'  o  MOLD shall be a scalar or an array of any type.', &
'', &
'  o  SIZE shall be a scalar of type integer.', &
'', &
'  o  RESULT has the same type as MOLD', &
'', &
'DESCRIPTION', &
'  TRANSFER(3) copies the bitwise representation of SOURCE in memory into a', &
'  variable or array of the same type and type parameters as MOLD.', &
'', &
'  This is approximately equivalent to the C concept of "casting" one type to', &
'  another.', &
'', &
'OPTIONS', &
'  o  SOURCE : Holds the bit pattern to be copied', &
'', &
'  o  MOLD : the type of MOLD is used to define the type of the returned value.', &
'     In addition, if it is an array the returned value is a one-dimensional', &
'     array. If it is a scalar the returned value is a scalar.', &
'', &
'  o  SIZE : If SIZE is present, the result is a one-dimensional array of', &
'     length SIZE.', &
'', &
'  If SIZE is absent but MOLD is an array (of any size or shape), the result is', &
'  a one-dimensional array of the minimum length needed to contain the entirety', &
'  of the bitwise representation of SOURCE.', &
'', &
'  If SIZE is absent and MOLD is a scalar, the result is a scalar.', &
'', &
'RESULT', &
'  The result has the bit level representation of SOURCE.', &
'', &
'  If the bitwise representation of the result is longer than that of SOURCE,', &
'  then the leading bits of the result correspond to those of SOURCE but any', &
'  trailing bits are filled arbitrarily.', &
'', &
'  When the resulting bit representation does not correspond to a valid', &
'  representation of a variable of the same type as MOLD, the results are', &
'  undefined, and subsequent operations on the result cannot be guaranteed to', &
'  produce sensible behavior. For example, it is possible to create logical', &
'  variables for which VAR and .not. var both appear to be true.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_transfer', &
'      use,intrinsic :: iso_fortran_env, only : int32, real32', &
'      integer(kind=int32) :: i = 2143289344', &
'      real(kind=real32)   :: x', &
'      character(len=10)   :: string', &
'      character(len=1)    :: chars(10)', &
'         x=transfer(i, 1.0)    ! prints "nan" on i686', &
'         ! the bit patterns are the same', &
'         write(*,''(b0,1x,g0)'')x,x ! create a NaN', &
'         write(*,''(b0,1x,g0)'')i,i', &
'', &
'         ! a string to an array of characters', &
'         string=''abcdefghij''', &
'         chars=transfer(string,chars)', &
'         write(*,''(*("[",a,"]":,1x))'')string', &
'         write(*,''(*("[",a,"]":,1x))'')chars', &
'      end program demo_transfer', &
'', &
'  Results:', &
'', &
'         1111111110000000000000000000000 NaN', &
'         1111111110000000000000000000000 2143289344', &
'         [abcdefghij]', &
'         [a] [b] [c] [d] [e] [f] [g] [h] [i] [j]', &
'', &
'COMMENTS', &
'  Joe Krahn: Fortran uses MOLDING rather than CASTING.', &
'', &
'  Casting, as in C, is an in-place reinterpretation. A cast is a device that', &
'  is built around an object to change its shape.', &
'', &
'  Fortran TRANSFER(3) reinterprets data out-of-place. It can be considered', &
'  MOLDING rather than casting. A MOLD is a device that confers a shape onto an', &
'  object placed into it.', &
'', &
'  The advantage of molding is that data is always valid in the context of the', &
'  variable that holds it. For many cases, a decent compiler should optimize', &
'  TRANSFER(3) into a simple assignment.', &
'', &
'  There are disadvantages of this approach. It is problematic to define a', &
'  union of data types because you must know the largest data object, which can', &
'  vary by compiler or compile options. In many cases, an EQUIVALENCE would be', &
'  far more effective, but Fortran Standards committees seem oblivious to the', &
'  benefits of EQUIVALENCE when used sparingly.', &
'', &
'STANDARD', &
'  Fortran 90', &
'', &
'SEE ALSO', &
'  ****(3)', &
'', &
'  fortran-lang intrinsic descriptions', &
'', &
'                               December 16, 2022            transfer(3fortran)', &
'']

shortname="transfer"
call process()


case('195','transpose')

textblock=[character(len=256) :: &
'', &
'transpose(3fortran)                                        transpose(3fortran)', &
'', &
'NAME', &
'  TRANSPOSE(3) - [ARRAY:MANIPULATION] Transpose an array of rank two', &
'', &
'SYNOPSIS', &
'  result = transpose(matrix)', &
'', &
'           function transpose(matrix)', &
'', &
'            type(TYPE(kind=KIND)            :: transpose(N,M)', &
'            type(TYPE(kind=KIND),intent(in) :: matrix(M,N)', &
'', &
'CHARACTERISTICS', &
'  o  MATRIX is an array of any type with a rank of two.', &
'', &
'  o  The result will be the same type and kind as MATRIX and the reversed', &
'     shape of the input array', &
'', &
'DESCRIPTION', &
'  TRANSPOSE(3) transposes an array of rank two.', &
'', &
'  An array is transposed by interchanging the rows and columns of the given', &
'  matrix. That is, element (i,j) of the result has the value of element (j,i)', &
'  of the input for all (i,j).', &
'', &
'OPTIONS', &
'  o  MATRIX : The array to transpose', &
'', &
'RESULT', &
'  The transpose of the input array. The result has the same type as MATRIX,', &
'  and has shape [ m, n ] if MATRIX has shape [ n, m ].', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_transpose', &
'      implicit none', &
'      integer,save :: xx(3,5)= reshape([&', &
'          1,  2,  3,  4,  5,    &', &
'         10, 20, 30, 40, 50,    &', &
'         11, 22, 33, 44, -1055  &', &
'       ],shape(xx),order=[2,1])', &
'', &
'      call print_matrix_int(''xx array:'',xx)', &
'      call print_matrix_int(''xx array transposed:'',transpose(xx))', &
'', &
'      contains', &
'', &
'      subroutine print_matrix_int(title,arr)', &
'      ! print small 2d integer arrays in row-column format', &
'      implicit none', &
'      character(len=*),intent(in)  :: title', &
'      integer,intent(in)           :: arr(:,:)', &
'      integer                      :: i', &
'      character(len=:),allocatable :: biggest', &
'         write(*,*)trim(title)  ! print title', &
'         biggest=''           ''  ! make buffer to write integer into', &
'         ! find how many characters to use for integers', &
'         write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'         ! use this format to write a row', &
'         biggest=''(" > [",*(i''//trim(biggest)//'':,","))''', &
'         ! print one row of array at a time', &
'         do i=1,size(arr,dim=1)', &
'            write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'            write(*,''(" ]")'')', &
'         enddo', &
'      end subroutine print_matrix_int', &
'', &
'      end program demo_transpose', &
'', &
'  Results:', &
'', &
'          xx array:', &
'          > [     1,     2,     3,     4,     5 ]', &
'          > [    10,    20,    30,    40,    50 ]', &
'          > [    11,    22,    33,    44, -1055 ]', &
'          xx array transposed:', &
'          > [     1,    10,    11 ]', &
'          > [     2,    20,    22 ]', &
'          > [     3,    30,    33 ]', &
'          > [     4,    40,    44 ]', &
'          > [     5,    50, -1055 ]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  o  MERGE(3) - Merge variables', &
'', &
'  o  PACK(3) - Pack an array into an array of rank one', &
'', &
'  o  SPREAD(3) - Add a dimension and replicate data', &
'', &
'  o  UNPACK(3) - Scatter the elements of a vector', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022           transpose(3fortran)', &
'']

shortname="transpose"
call process()


case('196','trim')

textblock=[character(len=256) :: &
'', &
'trim(3fortran)                                                  trim(3fortran)', &
'', &
'NAME', &
'  TRIM(3) - [CHARACTER:WHITESPACE] Remove trailing blank characters from a', &
'  string', &
'', &
'SYNOPSIS', &
'  result = trim(string)', &
'', &
'           character(len=:,kind=KIND) function trim(string)', &
'', &
'            character(len=*,kind=KIND),intent(in) :: string', &
'', &
'CHARACTERISTICS', &
'  o  KIND can be any kind supported for the character type.', &
'', &
'  o  The result has the same type and kind as the input argument STRING.', &
'', &
'DESCRIPTION', &
'  TRIM(3) removes trailing blank characters from a string.', &
'', &
'OPTIONS', &
'  o  STRING : A string to trim', &
'', &
'RESULT', &
'  The result is the same as STRING except trailing blanks are removed.', &
'', &
'  If STRING is composed entirely of blanks or has zero length, the result has', &
'  zero length.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'      program demo_trim', &
'      implicit none', &
'      character(len=:), allocatable :: str, strs(:)', &
'      character(len=*),parameter :: brackets=''( *("[",a,"]":,1x) )''', &
'      integer :: i', &
'', &
'         str=''   trailing    ''', &
'         print brackets, str,trim(str) ! trims it', &
'', &
'         str=''   leading''', &
'         print brackets, str,trim(str) ! no effect', &
'', &
'         str=''            ''', &
'         print brackets, str,trim(str) ! becomes zero length', &
'         print *,  len(str), len(trim(''               ''))', &
'', &
'        ! array elements are all the same length, so you often', &
'        ! want to print them', &
'         strs=[character(len=10) :: "Z"," a b c","ABC",""]', &
'', &
'         write(*,*)''untrimmed:''', &
'         ! everything prints as ten characters; nice for neat columns', &
'         print brackets, (strs(i), i=1,size(strs))', &
'         print brackets, (strs(i), i=size(strs),1,-1)', &
'         write(*,*)''trimmed:''', &
'         ! everything prints trimmed', &
'         print brackets, (trim(strs(i)), i=1,size(strs))', &
'         print brackets, (trim(strs(i)), i=size(strs),1,-1)', &
'', &
'      end program demo_trim', &
'', &
'  Results:', &
'', &
'          > [   trailing    ] [   trailing]', &
'          > [   leading] [   leading]', &
'          > [            ] []', &
'          >           12           0', &
'          >  untrimmed:', &
'          > [Z         ] [ a b c    ] [ABC       ] [          ]', &
'          > [          ] [ABC       ] [ a b c    ] [Z         ]', &
'          >  trimmed:', &
'          > [Z] [ a b c] [ABC] []', &
'          > [] [ABC] [ a b c] [Z]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3), VERIFY(3)', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022                trim(3fortran)', &
'']

shortname="trim"
call process()


case('197','ubound')

textblock=[character(len=256) :: &
'', &
'ubound(3fortran)                                              ubound(3fortran)', &
'', &
'NAME', &
'  UBOUND(3) - [ARRAY:INQUIRY] Upper dimension bounds of an array', &
'', &
'SYNOPSIS', &
'  result = ubound(array [,dim] [,kind] )', &
'', &
'           elemental TYPE(kind=KIND) function ubound(array,dim,kind)', &
'', &
'            TYPE(kind=KIND),intent(in)           :: array', &
'            integer(kind=**),intent(in),optional :: dim', &
'            integer(kind=**),intent(in),optional :: kind', &
'', &
'CHARACTERISTICS', &
'  o  ARRAY shall be assumed-rank or an array, of any type. It cannot be an', &
'     unallocated allocatable array or a pointer that is not associated.', &
'', &
'  o  DIM shall be a scalar integer. The corresponding actual argument shall', &
'     not be an optional dummy argument, a disassociated pointer, or an', &
'     unallocated allocatable.', &
'', &
'  o  KIND an integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'  o  The return value is of type integer and of kind KIND. If KIND is absent,', &
'     the return value is of default integer kind. The result is scalar if DIM', &
'     is present; otherwise, the result is an array of rank one and size n,', &
'     where n is the rank of ARRAY.', &
'', &
'  o  a kind designated as ** may be any supported kind for the type', &
'', &
'DESCRIPTION', &
'  UBOUND(3) returns the upper bounds of an array, or a single upper bound', &
'  along the DIM dimension.', &
'', &
'OPTIONS', &
'  o  ARRAY : The assumed-rank or array of any type whose upper bounds are to', &
'     be determined. If allocatable it must be allocated; if a pointer it must', &
'     be associated. If an assumed-size array, DIM must be present.', &
'', &
'  o  DIM : a specific dimension of ARRAY to determine the bounds of. If DIM is', &
'     absent, the result is an array of the upper bounds of ARRAY.  DIM is', &
'     required if ARRAY is an assumed-size array, and in that case must be less', &
'     than or equal to the rank of ARRAY.', &
'', &
'  o  KIND : indicates the kind parameter of the result. If absent, an integer', &
'     of the default kind is returned.', &
'', &
'RESULT', &
'  The return value is of type integer and of kind KIND. If KIND is absent, the', &
'  return value is of default integer kind.', &
'', &
'  If DIM is absent, the result is an array of the upper bounds of each', &
'  dimension of the ARRAY.', &
'', &
'  If DIM is present, the result is a scalar corresponding to the upper bound', &
'  of the array along that dimension.', &
'', &
'  If ARRAY is an expression rather than a whole array or array structure', &
'  component, or if it has a zero extent along the relevant dimension, the', &
'  upper bound is taken to be the number of elements along the relevant', &
'  dimension.', &
'', &
'  NOTE1 If ARRAY is assumed-rank and has rank zero, DIM cannot be present', &
'  since it cannot satisfy the requirement 1 <= DIM <= 0.', &
'', &
'EXAMPLES', &
'  Note this function should not be used on assumed-size arrays or in any', &
'  function without an explicit interface. Errors can occur if there is no', &
'  interface defined.', &
'', &
'  Sample program', &
'', &
'      ! program demo_ubound', &
'      module m2_bounds', &
'      implicit none', &
'', &
'      contains', &
'', &
'      subroutine msub(arr)', &
'      !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array', &
'      integer,intent(in) :: arr(:)', &
'         write(*,*)''MSUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'      end subroutine msub', &
'', &
'      end module m2_bounds', &
'      !', &
'      program demo_ubound', &
'      use m2_bounds, only : msub', &
'      implicit none', &
'      interface', &
'         subroutine esub(arr)', &
'         integer,intent(in) :: arr(:)', &
'         end subroutine esub', &
'      end interface', &
'      integer :: arr(-10:10)', &
'         write(*,*)''MAIN: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'         call csub()', &
'         call msub(arr)', &
'         call esub(arr)', &
'      contains', &
'      subroutine csub', &
'         write(*,*)''CSUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'      end subroutine csub', &
'', &
'      end', &
'', &
'      subroutine esub(arr)', &
'      implicit none', &
'      integer,intent(in) :: arr(:)', &
'         ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE', &
'         ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)', &
'         write(*,*)''ESUB: LOWER='',lbound(arr),''UPPER='',ubound(arr), &', &
'         & ''SIZE='',size(arr)', &
'      end subroutine esub', &
'      !end program demo_ubound', &
'', &
'  Results:', &
'', &
'       >  MAIN: LOWER=         -10 UPPER=          10 SIZE=          21', &
'       >  CSUB: LOWER=         -10 UPPER=          10 SIZE=          21', &
'       >  MSUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'       >  ESUB: LOWER=           1 UPPER=          21 SIZE=          21', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument Fortran 2003', &
'', &
'SEE ALSO', &
'  Array inquiry:', &
'', &
'  o  SIZE(3) - Determine the size of an array', &
'', &
'  o  RANK(3) - Rank of a data object', &
'', &
'  o  SHAPE(3) - Determine the shape of an array', &
'', &
'  o  LBOUND(3) - Lower dimension bounds of an array', &
'', &
'  CO_UBOUND(3), CO_LBOUND(3)', &
'', &
'  State Inquiry:', &
'', &
'  o  ALLOCATED(3) - Status of an allocatable entity', &
'', &
'  o  IS_CONTIGUOUS(3) - Test if object is contiguous', &
'', &
'  Kind Inquiry:', &
'', &
'  o  KIND(3) - Kind of an entity', &
'', &
'  Bit Inquiry:', &
'', &
'  o  STORAGE_SIZE(3) - Storage size in bits', &
'', &
'  o  BIT_SIZE(3) - Bit size inquiry function', &
'', &
'  o  BTEST(3) - Tests a bit of an integer value.', &
'', &
'  o  LBOUND(3),', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              ubound(3fortran)', &
'']

shortname="ubound"
call process()


case('198','unpack')

textblock=[character(len=256) :: &
'', &
'unpack(3fortran)                                              unpack(3fortran)', &
'', &
'NAME', &
'  UNPACK(3) - [ARRAY:CONSTRUCTION] Scatter the elements of a vector into an', &
'  array using a mask', &
'', &
'SYNOPSIS', &
'  result = unpack(vector, mask, field)', &
'', &
'           type(TYPE(kind=KIND)) unpack(vector, mask, field)', &
'', &
'            type(TYPE(kind=KIND)),intent(in) :: vector(:)', &
'            logical,intent(in)               :: mask(..)', &
'            type(TYPE(kind=KIND)),intent(in) :: field(..)', &
'', &
'CHARACTERISTICS', &
'  o  VECTOR is a rank-one array of any type', &
'', &
'  o  MASK is a logical array', &
'', &
'  o  FIELD is the same type and type parameters as VECTOR conformable with', &
'     MASK.', &
'', &
'  o  The result is an array of the same type and type parameters as VECTOR and', &
'     the same shape as MASK.', &
'', &
'DESCRIPTION', &
'  UNPACK(3) scatters the elements of VECTOR into a copy of an array FIELD of', &
'  any rank using .true. values from MASK in array element order to specify', &
'  placement of the VECTOR values.', &
'', &
'  So a copy of FIELD is generated with select elements replaced with values', &
'  from VECTOR. This allows for complex replacement patterns that would be', &
'  difficult when using array syntax or multiple assignment statements,', &
'  particularly when the replacements are conditional.', &
'', &
'OPTIONS', &
'  o  VECTOR : New values to place into specified locations in FIELD. It shall', &
'     have at least as many elements as MASK has .true. values.', &
'', &
'  o  MASK : Shall be an array that specifies which values in FIELD are to be', &
'     replaced with values from VECTOR.', &
'', &
'  o  FIELD : The input array to be altered.', &
'', &
'RESULT', &
'  The element of the result that corresponds to the ith true element of MASK,', &
'  in array element order, has the value VECTOR(I) for i = 1, 2, . .  ., t,', &
'  where t is the number of true values in MASK. Each other element has a value', &
'  equal to *field if *field is scalar or to the corresponding element of', &
'  *field if it is an array.', &
'', &
'  The resulting array corresponds to FIELD with .true. elements of MASK', &
'  replaced by values from VECTOR in array element order.', &
'', &
'EXAMPLES', &
'  Particular values may be "scattered" to particular positions in an array by', &
'  using', &
'', &
'    1 0 0', &
'', &
'  If M is the array', &
'    0 1 0 0 0 1', &
'', &
'    V is the array [1, 2, 3], . T .', &
'', &
'    and Q is the logical mask', &
'      T . .  . . T where "T" represents true and "." represents false, then', &
'      the result of', &
'', &
'    UNPACK (V, MASK = Q, FIELD = M) has the value', &
'', &
'      1 2 0 1 1 0 0 0 3', &
'', &
'    and the result of UNPACK (V, MASK = Q, FIELD = 0) has the value', &
'', &
'      0 2 0 1 0 0 0 0 3', &
'', &
'  Sample program:', &
'', &
'      program demo_unpack', &
'      implicit none', &
'      logical,parameter :: T=.true., F=.false.', &
'', &
'      integer :: vector(2)  = [1,1]', &
'', &
'      ! mask and field must conform', &
'      integer,parameter :: r=2, c=2', &
'      logical :: mask(r,c)  = reshape([ T,F,F,T ],[2,2])', &
'      integer :: field(r,c) = 0, unity(2,2)', &
'', &
'         ! basic usage', &
'         unity = unpack( vector, mask, field )', &
'         call print_matrix_int(''unity='', unity)', &
'', &
'         ! if FIELD is a scalar it is used to fill all the elements', &
'         ! not assigned to by the vector and mask.', &
'         call print_matrix_int(''scalar field'',         &', &
'         & unpack(                                     &', &
'         & vector=[ 1, 2, 3, 4 ],                      &', &
'         & mask=reshape([ T,F,T,F,F,F,T,F,T ], [3,3]), &', &
'         & field=0) )', &
'', &
'      contains', &
'', &
'         subroutine print_matrix_int(title,arr)', &
'         ! convenience routine:', &
'         ! just prints small integer arrays in row-column format', &
'         implicit none', &
'         character(len=*),intent(in)  :: title', &
'         integer,intent(in)           :: arr(:,:)', &
'         integer                      :: i', &
'         character(len=:),allocatable :: biggest', &
'', &
'            write(*,*)trim(title)', &
'            ! make buffer to write integer into', &
'            biggest=''           ''', &
'            ! find how many characters to use for integers', &
'            write(biggest,''(i0)'')ceiling(log10(real(maxval(abs(arr)))))+2', &
'            ! use this format to write a row', &
'            biggest=''("  [",*(i''//trim(biggest)//'':,","))''', &
'            ! print one row of array at a time', &
'            do i=1,size(arr,dim=1)', &
'               write(*,fmt=biggest,advance=''no'')arr(i,:)', &
'               write(*,''(" ]")'')', &
'            enddo', &
'         end subroutine print_matrix_int', &
'', &
'      end program demo_unpack', &
'', &
'  Results:', &
'', &
'         > unity=', &
'         >  [ 1, 0 ]', &
'         >  [ 0, 1 ]', &
'         > scalar field', &
'         >  [  1,  0,  3 ]', &
'         >  [  0,  0,  0 ]', &
'         >  [  2,  0,  4 ]', &
'', &
'STANDARD', &
'  Fortran 95', &
'', &
'SEE ALSO', &
'  MERGE(3), PACK(3), SPREAD(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              unpack(3fortran)', &
'']

shortname="unpack"
call process()


case('199','verify')

textblock=[character(len=256) :: &
'', &
'verify(3fortran)                                              verify(3fortran)', &
'', &
'NAME', &
'  VERIFY(3) - [CHARACTER:SEARCH] Position of a character in a string of', &
'  characters that does not appear in a given set of characters.', &
'', &
'SYNOPSIS', &
'  result = verify(string, set [,back] [,kind] )', &
'', &
'           elemental integer(kind=KIND) function verify(string,set,back,KIND)', &
'', &
'            character(len=*,kind=**),intent(in) :: string', &
'            character(len=*,kind=**),intent(in) :: set', &
'            logical,intent(in),optional :: back', &
'            integer,intent(in),optional :: KIND', &
'', &
'CHARACTERISTICS', &
'  o  STRING and SET must be of type character and have the same kind for any', &
'     individual call, but that can be any supported character kind.', &
'', &
'  o  KIND must be a constant integer initialization expression and a valid', &
'     kind for the integer type.', &
'', &
'  o  BACK shall be of type logical.', &
'', &
'  o  the kind of the returned value is the same as KIND if present.  Otherwise', &
'     a default integer kind is returned.', &
'', &
'DESCRIPTION', &
'  VERIFY(3) verifies that all the characters in STRING belong to the set of', &
'  characters in SET by identifying the position of the first character in the', &
'  string that is not in the set.', &
'', &
'  This makes it easy to verify strings are all uppercase or lowercase, follow', &
'  a basic syntax, only contain printable characters, and many of the', &
'  conditions tested for with the C routines ISALNUM(3c), ISALPHA(3c),', &
'  ISASCII(3c), ISBLANK(3c), ISCNTRL(3c), ISDIGIT(3c), ISGRAPH(3c),', &
'  ISLOWER(3c), ISPRINT(3c), ISPUNCT(3c), ISSPACE(3c), ISUPPER(3c), and', &
'  ISXDIGIT(3c); but for a string as well as an array of strings.', &
'', &
'OPTIONS', &
'  o  STRING : The string to search in for an unmatched character.', &
'', &
'  o  SET : The set of characters that must be matched.', &
'', &
'  o  BACK : The direction to look for an unmatched character. The left-most', &
'     unmatched character position is returned unless BACK is present and', &
'     .false., which causes the position of the right-most unmatched character', &
'     to be returned instead of the left-most unmatched character.', &
'', &
'  o  KIND : An integer initialization expression indicating the kind parameter', &
'     of the result.', &
'', &
'RESULT', &
'  If all characters of STRING are found in SET, the result is zero.', &
'', &
'  If STRING is of zero length a zero (0) is always returned.', &
'', &
'  Otherwise, if an unmatched character is found The position of the first or', &
'  last (if BACK is .false.) unmatched character in STRING is returned,', &
'  starting with position one on the left end of the string.', &
'', &
'EXAMPLES', &
'  Sample program I:', &
'', &
'      program demo_verify', &
'      implicit none', &
'      ! some useful character sets', &
'      character,parameter :: &', &
'       & int*(*)   = ''1234567890'', &', &
'       & low*(*)   = ''abcdefghijklmnopqrstuvwxyz'', &', &
'       & upp*(*)   = ''ABCDEFGHIJKLMNOPQRSTUVWXYZ'', &', &
'       & punc*(*)  = "!""#$%&''()*+,-./:;<=>?@[\]^_`{|}~", &', &
'       & blank*(*) = '' '', &', &
'       & tab       = char(11), &', &
'       & prnt*(*) = int//low//upp//blank//punc', &
'', &
'      character(len=:),allocatable :: string', &
'      integer :: i', &
'          print *, ''basics:''', &
'          print *, VERIFY (''ABBA'', ''A'')                ! has the value 2.', &
'          print *, VERIFY (''ABBA'', ''A'', BACK = .TRUE.) ! has the value 3.', &
'          print *, VERIFY (''ABBA'', ''AB'')               ! has the value 0.', &
'', &
'         print *,''find first non-uppercase letter''', &
'         ! will produce the location of "d", because there is no match in UPP', &
'         write(*,*) ''something unmatched'',verify("ABCdEFG", upp)', &
'', &
'         print *,''if everything is matched return zero''', &
'         ! will produce 0 as all letters have a match', &
'         write(*,*) ''everything matched'',verify("ffoorrttrraann", "nartrof")', &
'', &
'         print *,''easily categorize strings as uppercase, lowercase, ...''', &
'         ! easy C-like functionality but does entire strings not just characters', &
'         write(*,*)''isdigit 123?'',verify("123", int) == 0', &
'         write(*,*)''islower abc?'',verify("abc", low) == 0', &
'         write(*,*)''isalpha aBc?'',verify("aBc", low//upp) == 0', &
'         write(*,*)''isblank aBc dEf?'',verify("aBc dEf", blank//tab ) /= 0', &
'         ! check if all printable characters', &
'         string="aB;cde,fgHI!Jklmno PQRSTU vwxyz"', &
'         write(*,*)''isprint?'',verify(string,prnt) == 0', &
'         ! this now has a nonprintable tab character in it', &
'         string(10:10)=char(11)', &
'         write(*,*)''isprint?'',verify(string,prnt) == 0', &
'', &
'         print *,''VERIFY(3) is very powerful using expressions as masks''', &
'         ! verify(3f) is often used in a logical expression', &
'         string=" This is NOT all UPPERCASE "', &
'         write(*,*)''all uppercase/spaces?'',verify(string, blank//upp) == 0', &
'         string=" This IS all uppercase "', &
'         write(*,*) ''string=[''//string//'']''', &
'         write(*,*)''all uppercase/spaces?'',verify(string, blank//upp) == 0', &
'', &
'        ! set and show complex string to be tested', &
'         string=''  Check this out. Let me know  ''', &
'         ! show the string being examined', &
'         write(*,*) ''string=[''//string//'']''', &
'         write(*,*) ''        ''//repeat(int,4) ! number line', &
'', &
'         ! the Fortran functions returns a position just not a logical like C', &
'         print *, ''returning a position not just a logical is useful''', &
'         ! which can be very useful for parsing strings', &
'         write(*,*)''first non-blank character'',verify(string, blank)', &
'         write(*,*)''last non-blank character'',verify(string, blank,back=.true.)', &
'         write(*,*)''first non-letter non-blank'',verify(string,low//upp//blank)', &
'', &
'        !VERIFY(3) is elemental so you can check an array of strings in one call', &
'        print *, ''elemental''', &
'         ! are strings all letters (or blanks)?', &
'         write(*,*) ''array of strings'',verify( &', &
'         ! strings must all be same length, so force to length 10', &
'         & [character(len=10) :: "YES","ok","000","good one","Nope!"], &', &
'         & low//upp//blank) == 0', &
'', &
'         ! rarer, but the set can be an array, not just the strings to test', &
'         ! you could do ISPRINT() this (harder) way :>', &
'         write(*,*)''isprint?'',.not.all(verify("aBc", [(char(i),i=32,126)])==1)', &
'         ! instead of this way', &
'         write(*,*)''isprint?'',verify("aBc",prnt) == 0', &
'', &
'      end program demo_verify', &
'', &
'  Results:', &
'', &
'       >  basics:', &
'       >            2', &
'       >            3', &
'       >            0', &
'       >  find first non-uppercase letter', &
'       >  something unmatched           4', &
'       >  if everything is matched return zero', &
'       >  everything matched           0', &
'       >  easily categorize strings as uppercase, lowercase, ...', &
'       >  isdigit 123? T', &
'       >  islower abc? T', &
'       >  isalpha aBc? T', &
'       >  isblank aBc dEf? T', &
'       >  isprint? T', &
'       >  isprint? F', &
'       >  VERIFY(3) is very powerful using expressions as masks', &
'       >  all uppercase/spaces? F', &
'       >  string=[ This IS all uppercase ]', &
'       >  all uppercase/spaces? F', &
'       >  string=[  Check this out. Let me know  ]', &
'       >          1234567890123456789012345678901234567890', &
'       >  returning a position not just a logical is useful', &
'       >  first non-blank character           3', &
'       >  last non-blank character          29', &
'       >  first non-letter non-blank          17', &
'       >  elemental', &
'       >  array of strings T T F T F', &
'       >  isprint? T', &
'       >  isprint? T', &
'', &
'  Sample program II:', &
'', &
'  Determine if strings are valid integer representations', &
'', &
'      program fortran_ints', &
'      implicit none', &
'      integer :: i', &
'      character(len=*),parameter :: ints(*)=[character(len=10) :: &', &
'       ''+1 '', &', &
'       ''3044848 '', &', &
'       ''30.40 '', &', &
'       ''September '', &', &
'       ''1 2 3'', &', &
'       ''  -3000 '', &', &
'       '' '']', &
'         ! show the strings to test', &
'         write(*,''("|",*(g0,"|"))'') ints', &
'         ! show if strings pass or fail the test done by isint(3f)', &
'         write(*,''("|",*(1x,l1,8x,"|"))'') isint(ints)', &
'', &
'      contains', &
'', &
'      elemental function isint(line) result (lout)', &
'      !', &
'      ! determine if string is a valid integer representation', &
'      ! ignoring trailing spaces and leading spaces', &
'      !', &
'      character(len=*),parameter   :: digits=''0123456789''', &
'      character(len=*),intent(in)  :: line', &
'      character(len=:),allocatable :: name', &
'      logical                      :: lout', &
'         lout=.false.', &
'         ! make sure at least two characters long to simplify tests', &
'         name=adjustl(line)//''  ''', &
'         ! blank string', &
'         if( name == '''' )return', &
'         ! allow one leading sign', &
'         if( verify(name(1:1),''+-'') == 0 ) name=name(2:)', &
'         ! was just a sign', &
'         if( name == '''' )return', &
'         lout=verify(trim(name), digits)  == 0', &
'      end function isint', &
'', &
'      end program fortran_ints', &
'', &
'  Results:', &
'', &
'      |+1       |3044848  |30.40    |September|1 2 3    |  -3000  |         |', &
'      | T       | T       | F       | F       | F       | T       | F       |', &
'', &
'  Sample program III:', &
'', &
'  Determine if strings represent valid Fortran symbol names', &
'', &
'      program fortran_symbol_name', &
'      implicit none', &
'      integer :: i', &
'      character(len=*),parameter :: symbols(*)=[character(len=10) :: &', &
'       ''A_ '', &', &
'       ''10 '', &', &
'       ''September '', &', &
'       ''A B'', &', &
'       ''_A '', &', &
'       '' '']', &
'', &
'         write(*,''("|",*(g0,"|"))'') symbols', &
'         write(*,''("|",*(1x,l1,8x,"|"))'') fortran_name(symbols)', &
'', &
'      contains', &
'', &
'      elemental function fortran_name(line) result (lout)', &
'      !', &
'      ! determine if a string is a valid Fortran name', &
'      ! ignoring trailing spaces (but not leading spaces)', &
'      !', &
'      character(len=*),parameter   :: int=''0123456789''', &
'      character(len=*),parameter   :: lower=''abcdefghijklmnopqrstuvwxyz''', &
'      character(len=*),parameter   :: upper=''ABCDEFGHIJKLMNOPQRSTUVWXYZ''', &
'      character(len=*),parameter   :: allowed=upper//lower//int//''_''', &
'', &
'      character(len=*),intent(in)  :: line', &
'      character(len=:),allocatable :: name', &
'      logical                      :: lout', &
'         name=trim(line)', &
'         if(len(name).ne.0)then', &
'            ! first character is alphameric', &
'            lout = verify(name(1:1), lower//upper) == 0  &', &
'             ! other characters are allowed in a symbol name', &
'             & .and. verify(name,allowed) == 0           &', &
'             ! allowable length', &
'             & .and. len(name) <= 63', &
'         else', &
'            lout = .false.', &
'         endif', &
'      end function fortran_name', &
'', &
'      end program fortran_symbol_name', &
'', &
'  Results:', &
'', &
'          |A_        |10        |September |A B       |_A        |          |', &
'          | T        | F        | T        | F        | F        | F        |', &
'', &
'  Sample program IV:', &
'', &
'  check if string is of form NN-HHHHH', &
'', &
'      program checkform', &
'      ! check if string is of form NN-HHHHH', &
'      implicit none', &
'      character(len=*),parameter :: int=''1234567890''', &
'      character(len=*),parameter :: hex=''abcdefABCDEF0123456789''', &
'      logical                    :: lout', &
'      character(len=80)          :: chars', &
'', &
'         chars=''32-af43d''', &
'         lout=.true.', &
'', &
'         ! are the first two characters integer characters?', &
'         lout = lout.and.(verify(chars(1:2), int) == 0)', &
'', &
'         ! is the third character a dash?', &
'         lout = lout.and.(verify(chars(3:3), ''-'') == 0)', &
'', &
'         ! is remaining string a valid representation of a hex value?', &
'         lout = lout.and.(verify(chars(4:8), hex) == 0)', &
'', &
'         if(lout)then', &
'            write(*,*)trim(chars),'' passed''', &
'         else', &
'            write(*,*)trim(chars),'' failed''', &
'         endif', &
'      end program checkform', &
'', &
'  Results:', &
'', &
'          32-af43d passed', &
'', &
'  Sample program V:', &
'', &
'  exploring uses of elemental functionality and dusty corners', &
'', &
'      program more_verify', &
'      implicit none', &
'      character(len=*),parameter :: &', &
'        & int=''0123456789'', &', &
'        & low=''abcdefghijklmnopqrstuvwxyz'', &', &
'        & upp=''ABCDEFGHIJKLMNOPQRSTUVWXYZ'', &', &
'        & blank='' ''', &
'      ! note character variables in an array have to be of the same length', &
'      character(len=6) :: strings(3)=["Go    ","right ","home! "]', &
'      character(len=2) :: sets(3)=["do","re","me"]', &
'', &
'        ! elemental -- you can use arrays for both strings and for sets', &
'', &
'         ! check each string from right to left for non-letter/non-blank', &
'         write(*,*)''last non-letter'',verify(strings,upp//low//blank,back=.true.)', &
'', &
'         ! even BACK can be an array', &
'         ! find last non-uppercase character in "Howdy "', &
'         ! and first non-lowercase in "there "', &
'         write(*,*) verify(strings(1:2),[upp,low],back=[.true.,.false.])', &
'', &
'         ! using a null string for a set is not well defined. Avoid it', &
'         write(*,*) ''null'',verify("for tran ", "", .true.) ! 8,length of string?', &
'         ! probably what you expected', &
'         write(*,*) ''blank'',verify("for tran ", " ", .true.) ! 7,found ''n''', &
'', &
'         ! first character in  "Go    " not in "do",', &
'         ! and first letter in "right " not in "ri"', &
'         ! and first letter in "home! " not in "me"', &
'         write(*,*) verify(strings,sets)', &
'', &
'      end program more_verify', &
'', &
'  Results:', &
'', &
'          > last non-letter 0 0 5', &
'          > 6 6', &
'          > null 9', &
'          > blank 8', &
'          > 1 2 1', &
'', &
'STANDARD', &
'  Fortran 95 , with KIND argument - Fortran 2003', &
'', &
'SEE ALSO', &
'  Functions that perform operations on character strings, return lengths of', &
'  arguments, and search for certain arguments:', &
'', &
'  o  ELEMENTAL: ADJUSTL(3), ADJUSTR(3), INDEX(3), SCAN(3),', &
'', &
'  o  NONELEMENTAL: LEN_TRIM(3), LEN(3), REPEAT(3), TRIM(3)', &
'', &
'  fortran-lang intrinsic descriptions (license: MIT) @urbanjost', &
'', &
'                               December 16, 2022              verify(3fortran)', &
'']

shortname="verify"
call process()

case default
   allocate (character(len=256) :: textblock(0))
end select
contains
subroutine process()
if(present(topic))then
   if(topic)then
      textblock=[character(len=len(shortname)) :: shortname]
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
end subroutine process
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

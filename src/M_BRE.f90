!09/22/1980  15:38:34
!04/19/2020  11:05:06
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    M_bre(3fp) - [M_BRE] Basic Regular Expressions
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    use M_bre, only: match, amatch, getpat, makpat
!!    use M_bre, only: YES, MAXPAT, MAXARG, MAXLINE, EOS, NEWLINE, ERR
!!
!!##DESCRIPTION
!!    Find a string matching a regular expression.
!!
!!       *   zero or more occurrences of the previous character
!!       .   any character
!!       ^   beginning of line
!!       $   end of line
!!       []  class of characters. Inside the braces
!!
!!            ^  at the beginning of the class means to
!!               negate the class.
!!            -  if not the first or last character in
!!               the class, denotes a range of characters
!!       Escape characters:
!!        \\n  newline
!!        \\r  carriage return
!!        \\t  tab
!!        \\b  backspace
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
module M_BRE
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none

! ident_1="@(#)M_BRE::M_BRE(3fm): module for Basic Regular Expressions, inspired by Ratfor version"

private

! allow ratfor strings and Fortran strings

public   match;   interface  match;   module  procedure  match_,   match__;   end  interface
public   getpat;  interface  getpat;  module  procedure  getpat_,  getpat__;  end  interface
public   amatch;  interface  amatch;  module  procedure  amatch_,  amatch__;  end  interface
public   makpat;  interface  makpat;  module  procedure  makpat_   ;          end  interface
public   error;   interface  error;   module  procedure  error_    ;          end  interface
private  addset;  interface  addset;  module  procedure  addset_   ;          end  interface
private  dodash;  interface  dodash;  module  procedure  dodash_   ;          end  interface
private  esc;     interface  esc;     module  procedure  esc_      ;          end  interface
private  filset;  interface  filset;  module  procedure  filset_   ;          end  interface
private  getccl;  interface  getccl;  module  procedure  getccl_   ;          end  interface
private  indx;    interface  indx;    module  procedure  indx_     ;          end  interface
private  locate;  interface  locate;  module  procedure  locate_   ;          end  interface
private  omatch;  interface  omatch;  module  procedure  omatch_   ;          end  interface
private  patsiz;  interface  patsiz;  module  procedure  patsiz_   ;          end  interface
private  stclos;  interface  stclos;  module  procedure  stclos_   ;          end  interface

public test_suite_M_BRE

public YES
public NO
public MAXPAT
public MAXARG
public MAXLINE
public MAXTAGS
public EOS
public NEWLINE
public ERR

integer,parameter :: MAXPAT=1024
integer,parameter :: MAXARG=1024
integer,parameter :: MAXLINE=1024
integer,parameter :: MAXTAGS=10

public regex_pattern
type :: regex_pattern
   integer :: pat(MAXPAT)
end type regex_pattern

integer,parameter :: CLOSURE=ichar('*') ! *    *
integer,parameter :: EOL=ichar('$')     ! $    $
integer,parameter :: ANY=ichar('.')     ! ?    .
integer,parameter :: BOL=ichar('^')     ! %    ^
integer,parameter :: NCCL=ichar('n')    ! n    n
integer,parameter :: NOT=ichar('^')     ! ~    ~  processed only in a CCL, so can be same as BOL, which is only special when first
integer,parameter :: CCL=ichar('[')     ! [    [
integer,parameter :: CCLEND=ichar(']')  ! ]    ]
integer,parameter :: ESCAP=ichar('\')   ! @    \
!! OTHER CALLS START_TAG BOSS AND STOP_TAG EOSS AND USES < AND >
integer,parameter :: START_TAG=ichar('{')  ! {    [
integer,parameter :: STOP_TAG=ichar('}')   ! }    ]
!! DUPLICATE
integer,parameter :: BOSS=ichar('{')  ! {    [
integer,parameter :: EOSS=ichar('}')  ! }    ]

integer,parameter :: DASH=ichar('-')
integer,parameter :: CHARA=ichar('a')
integer,parameter :: LETB=ichar('B'), CHARB=ichar('b')
integer,parameter :: LETN=ichar('N'), CHARN=ichar('n')
integer,parameter :: LETR=ichar('R'), CHARR=ichar('r')
integer,parameter :: LETT=ichar('T'), CHART=ichar('t')
integer,parameter :: NEWLINE=10
integer,parameter :: BACKSPACE=8
integer,parameter :: HT=9   ! horizontal tab
integer,parameter :: CR=13  ! carriage return
integer,parameter :: BADCHAR=0
integer,parameter :: EOS=-2
integer,parameter :: ERR=-3

integer,parameter :: START=3
integer,parameter :: CLOSIZE=4
integer,parameter :: NO=0
integer,parameter :: YES=1

integer,parameter :: DIG0=48
integer,parameter :: DIG1=49
integer,parameter :: DIG2=50
integer,parameter :: DIG3=51
integer,parameter :: DIG4=52
integer,parameter :: DIG5=53
integer,parameter :: DIG6=54
integer,parameter :: DIG7=55
integer,parameter :: DIG8=56
integer,parameter :: DIG9=57
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    locate(3f) - [M_BRE] look for c in char class at pat(offset)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     pure integer function locate(c, pat, offset)
!!##DESCRIPTION
!!    look for c in char class at pat(offset)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
pure function locate_(c, pat, offset)
implicit none

! ident_2="@(#)M_BRE::locate(3f): look for c in char class at pat(offset)"

integer            :: locate_
integer,intent(in) :: c
integer,intent(in) :: pat(MAXPAT)
integer,intent(in) :: offset
integer            :: i
   ! size of class is at pat(offset), characters follow
   locate_=NO
   LOC: do i = offset + pat(offset), offset, -1
      if(c .eq. pat(i)) then
         locate_=YES
         exit LOC
      endif
   enddo LOC
end function locate_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getpat(3f) - [M_BRE] convert str into pattern
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function getpat(str, pat)
!!##DESCRIPTION
!!    convert str into pattern
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function getpat__(string, pat)

! ident_3="@(#)M_BRE::getpat(3f): convert str into pattern"

character(len=*),intent(in) :: string
integer,intent(out)         :: pat(*)
integer                     :: str(MAXPAT)
integer                     :: len_string
   len_string=len(string)
   if(len_string.gt.MAXARG-1)then
      write(*,*)'*getpat* error: input string too long,',len_string,' > ',MAXARG-1
      stop
   else
      str(1:len_string+1)=f2r(string) ! convert string to ADE array
      getpat__=getpat_(str, pat)
   endif
end function getpat__
integer function getpat_(str, pat)

! ident_4="@(#)M_BRE::getpat(3f): convert str into pattern"

integer,intent(in)  :: str(*)
integer             :: pat(*)
   getpat_=makpat_(str, 1, EOS, pat)
end function getpat_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    addset(3f) - [M_BRE] put c in string(j) if it fits, increment
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function addset(c, str, j, maxsiz)
!!##DESCRIPTION
!!   put c in string(j) if it fits, increment
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function addset_(c, string, j, maxsiz)

! ident_5="@(#)M_BRE::addset(3f): put c in string(j) if it fits, increment"

   integer            :: addset_
   integer,intent(in) :: c
   integer,intent(in) :: maxsiz
   integer :: string(maxsiz)
   integer :: j
   if(j .gt. maxsiz)then
      addset_ = NO
   else
      string(j) = c
      j = j + 1
      addset_ = YES
   endif
end function addset_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    stclos(3f) - [M_BRE] insert CLOSURE entry at pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function stclos(pat, j, lastj, lastcl)
!!##DESCRIPTION
!!  insert CLOSURE entry at pat(j)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function stclos_(pat, j, lastj, lastcl)

! ident_6="@(#)M_BRE::stclos(3f): insert CLOSURE entry at pat(j)"

integer pat(MAXPAT)
integer j, lastj, lastcl
integer jp, jt, junk
   do jp=j-1, lastj, -1 ! make a hole
      jt = jp + CLOSIZE
      junk = addset_(pat(jp), pat, jt, MAXPAT)
   enddo
   j = j + CLOSIZE
   stclos_ = lastj
   !! OTHER USES 0 INSTEAD OF BADCHAR
   junk = addset_(CLOSURE, pat, lastj, MAXPAT)  ! put closure in it
   junk = addset_(BADCHAR, pat, lastj, MAXPAT)  ! COUNT
   junk = addset_(lastcl,  pat, lastj, MAXPAT)  ! PREVCL
   junk = addset_(BADCHAR, pat, lastj, MAXPAT)  ! START
end function stclos_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    dodash(3f) - [M_BRE] expand array(i-1)-array(i+1) into set(j)... from valid
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine dodash(valid, array, i, set, j, maxset)
!!##DESCRIPTION
!!    expand array(i-1)-array(i+1) into set(j)... from valid
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
subroutine dodash_(valid, array, i, set, j, maxset)

! ident_7="@(#)M_BRE::dodash(3f): expand array(i-1)-array(i+1) into set(j)... from valid"

integer :: valid(*)
integer :: array(*)
integer :: i
integer :: maxset
integer :: set(maxset)
integer :: j
integer :: junk
integer :: k
integer :: limit
   i = i + 1
   j = j - 1
   limit = indx_(valid, esc_(array, i))
   do k = indx_(valid, set(j)), limit
      junk = addset_(valid(k), set, j, maxset)
   enddo
end subroutine dodash_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getccl(3f) - [M_BRE] expand char class at arg(i) into pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function getccl(arg, i, pat, j)
!!##DESCRIPTION
!!    expand char class at arg(i) into pat(j)
!!##OPTIONS
!!    ARG  ADE string array
!!    I    index into ARG
!!    PAT  encoded regular expression
!!    J    .
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function getccl_(arg, i, pat, j)

! ident_8="@(#)M_BRE::getccl(3f): expand char class at arg(i) into pat(j)"

integer :: arg(MAXARG)
integer :: i
integer :: pat(MAXPAT)
integer :: j

integer :: jstart
integer :: junk

   i = i + 1                                ! skip over [

   if(arg(i) .eq. NOT)then !! OTHER SAYS TILDE or CARET INSTEAD OF NOT
      junk = addset_(NCCL, pat, j, MAXPAT)
      i = i + 1
   else
      junk = addset_(CCL, pat, j, MAXPAT)
   endif

   jstart = j
   junk = addset_(BADCHAR, pat, j, MAXPAT)         ! leave room for count
   call filset_(CCLEND, arg, i, pat, j, MAXPAT)
   pat(jstart) = j - jstart - 1

   if(arg(i) .eq. CCLEND)then
      getccl_ = YES  !! OTHER SAYS = EOS, OTHER SAYS OK
   else
      getccl_ = ERR
   endif

end function getccl_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    filset(3f) - [M_BRE] expand set at array(i) into set(j), stop at delim
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine filset(delim, array, i, set, j, maxset)
!!##DESCRIPTION
!!   expand set at array(i) into set(j), stop at delim
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
subroutine filset_(delim, array, i, set, j, maxset)

! ident_9="@(#)M_BRE::filset(3f): expand set at array(i) into set(j), stop at delim"

   integer           :: delim
   integer           :: array(*)
   integer           :: i
   integer           :: maxset
   integer           :: set(maxset)
   integer           :: j

   integer           :: junk
   integer           :: k
   integer,parameter :: digits(11)=[(k,k=48,57),EOS]
   integer,parameter :: lowalf(27)=[(k,k=97,122),EOS]
   integer,parameter :: upalf(27)=[(k,k=65,90),EOS]

   do while( array(i) /= delim .and. array(i) /= EOS)
      if(array(i) .eq. ESCAP)then
         junk = addset_(esc_(array, i), set, j, maxset)
      elseif(array(i) .ne. DASH)then
         junk = addset_(array(i), set, j, maxset)

      ! specifically do digit-digit, lowercase-lowercase, uppercase-uppercase
      ! some just assume ASCII character set and allow range between any two characters
      elseif(j .le. 1 .or. array(i + 1) .eq. EOS)then  ! literal - at beginning or end
         junk = addset_(DASH, set, j, maxset)
      elseif(indx_(digits, set(j - 1)) .gt. 0)then
         call dodash_(digits, array, i, set, j, maxset)
      elseif(indx_(lowalf, set(j - 1)) .gt. 0)then
         call dodash_(lowalf, array, i, set, j, maxset)
      elseif(indx_(upalf, set(j - 1)) .gt. 0)then
         call dodash_(upalf, array, i, set, j, maxset)
      else
         junk = addset_(DASH, set, j, maxset)
      endif
      i = i + 1
   enddo
end subroutine filset_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    indx(3f) - [M_BRE] returns position of character in string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function indx(str, c)
!!##DESCRIPTION
!!  returns position of character in string
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function indx_(str, c)

! ident_10="@(#)M_BRE::indx(3f): returns position of character in string"

integer :: str(*)
integer :: c

   indx_ = 1
   INFINITE: do
      if(str(indx_) .eq. EOS)exit INFINITE
      if(str(indx_) .eq. c)then
         return
      endif
      indx_ = indx_ + 1
   enddo INFINITE
   indx_ = 0
end function indx_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    match(3f) - [M_BRE] find match to a basic regular expression anywhere on input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function match(line, pattern)
!!
!!     character(len=*),intent(in) :: line
!!     integer,intent(in)          :: pattern(MAXPAT)
!!
!!##DESCRIPTION
!!  Given a BRE(Basic Regular Expression) converted to a pattern
!!  return whether an input string matches it.
!!
!!##OPTIONS
!!    LIN  string to search for a match to the pattern
!!    PAT  pattern generated from a BRE using getpat(3f) or makpat(3f).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_match
!!     use :: M_BRE, only : getpat, match
!!     use :: M_BRE, only : MAXPAT, MAXARG, MAXLINE, YES, ERR
!!     implicit none
!!     ! find _ find patterns in text
!!     integer                      :: pat(MAXPAT)
!!     character(len=MAXARG-1)      :: argument
!!     integer                      :: stat
!!     integer                      :: ios
!!     integer                      :: len_arg
!!     character(len=MAXLINE-2)     :: line
!!     call get_command_argument(1, argument,status=stat,length=len_arg)
!!     if(stat.ne.0.or.argument.eq.'')then
!!        write(*,*)"usage: find pattern."
!!     elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
!!        write(*,*)"illegal pattern."
!!     else
!!        INFINITE: do
!!           read(*,'(a)',iostat=ios)line
!!           if(ios.ne.0)exit
!!           if(match(trim(line), pat) .eq. YES) then
!!              write(*,'(*(a))')trim(line)
!!           endif
!!        enddo INFINITE
!!     endif
!!     end program demo_match
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!
!!##LICENSE
!!   Public Domain
integer function match__(line,pat)
character(len=*),intent(in) :: line
integer,intent(in)          :: pat(MAXPAT)
integer                     :: lin(MAXLINE)
integer                     :: k
integer                     :: len_line
   len_line=len(line)
   if(len_line.gt.MAXLINE-2)then
      write(*,*)'*match* error: input line too long, > ',MAXLINE-2
      stop
   else
      lin=0
      if(len_line.ne.0)then
         do k=1,len_line
            lin(k)=ichar(line(k:k))
         enddo
      endif
      lin(len_line+1:len_line+1)=NEWLINE
      lin(len_line+2:len_line+2)=EOS
      match__=match_(lin, pat)
   endif
end function match__
integer function match_(lin, pat)

! ident_11="@(#)M_BRE::match(3f): find match anywhere on line"

   integer :: lin(MAXLINE)
   integer :: pat(MAXPAT)
   integer :: i
   integer :: junk(MAXTAGS)
   i = 1
   match_ = NO
!! OTHER  if(pat(1) == bol) then            ! anchored match
!! OTHER    if(amatch_(lin, 1, pat) > 0) then
!! OTHER        match_ = yes
!! OTHER        return
!! OTHER     endif
!! OTHER  else ! unanchored match
   do while(lin(i) /= EOS)
      if(amatch_(lin, i, pat, junk, junk) .gt. 0)then
         match_ = YES
         exit
      endif
      i = i + 1
   enddo
!! OTHER  endif

end function match_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    error(3f) - [M_BRE] print message and stop program execution
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine error(line)
!!##DESCRIPTION
!!  print message and stop program execution
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
subroutine error_(line)

! ident_12="@(#)M_BRE::error(3f): print message and stop program execution"

character(len=*),intent(in) ::  line
   write(*,'(a)') line
   stop
end subroutine error_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    patsiz(3f) - [M_BRE] returns size of pattern entry at pat(n)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function patsiz(pat, n)
!!##DESCRIPTION
!!  returns size of pattern entry at pat(n)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function patsiz_(pat, n)

! ident_13="@(#)M_BRE::patsiz(3f): returns size of pattern entry at pat(n)"

   integer :: pat(MAXPAT)
   integer :: n

   select case(pat(n))
    case(CHARA,START_TAG,STOP_TAG)
      patsiz_ = 2
    case(BOL,EOL,ANY)
      patsiz_ = 1
    case(CCL,NCCL)
      patsiz_ = pat(n + 1) + 2
    case(CLOSURE)                  ! optional
      patsiz_ = CLOSIZE
    case default
      call error("in patsiz: cannot happen.")
   end select

end function patsiz_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    makpat(3f) - [M_BRE] make pattern from arg(from), terminate on delim
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function makpat(arg, from, delim, pat)
!!##DESCRIPTION
!!  make pattern from arg(from), terminate on delim
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function makpat_(arg, from, delim, pat)

! ident_14="@(#)M_BRE::makpat(3f): make pattern from arg(from), terminate on delim"

   integer arg(MAXARG)
   integer from
   integer delim
   integer pat(MAXPAT)
   integer i, j, junk, lastcl, lastj, lj, tagnst, tagnum, tagstk(MAXTAGS)

   j = 1        ! pat index
   lastj = 1
   lastcl = BADCHAR !! OTHER SETS THIS TO -1
   tagnum = 0
   tagnst = 0
   i = from
   MAKPATIT: do while(arg(i) /= delim .and. arg(i) /= EOS)
      lj = j
      if(arg(i) .eq. ANY)then
         junk = addset_(ANY, pat, j, MAXPAT)
      elseif(arg(i) .eq. BOL .and. i .eq. from)then
         junk = addset_(BOL, pat, j, MAXPAT)
      elseif(arg(i) .eq. EOL .and. arg(i + 1) .eq. delim)then
         junk = addset_(EOL, pat, j, MAXPAT)
      elseif(arg(i) .eq. CCL)then
         if(getccl_(arg, i, pat, j) .eq. ERR)then
            makpat_ = ERR
            return  !! OTHERS SAY EXIT
         endif
      elseif(arg(i) .eq. CLOSURE .and. i .gt. from)then
         lj = lastj
         !! OTHER if(pat(lj).eq.BOL.or.pat(lj).eq.EOL.or.pat(lj).eq.CLOSURE.or.pat(lj-1).eq.START_TAG.or.pat(lj-1).eq.STOP_TAG)then
         if(pat(lj).eq.BOL.or.pat(lj).eq.EOL.or.pat(lj).eq.CLOSURE.or.pat(lj).eq.START_TAG.or.pat(lj).eq.STOP_TAG)then
            exit MAKPATIT
         endif
         lastcl = stclos_(pat, j, lastj, lastcl)
      elseif(arg(i) .eq. START_TAG)then
         if(tagnum .ge. MAXTAGS)then
            ! too many tagged sub-patterns
            exit MAKPATIT
         endif
         tagnum = tagnum + 1
         tagnst = tagnst + 1
         tagstk(tagnst) = tagnum
         junk = addset_(START_TAG, pat, j, MAXPAT)
         junk = addset_(tagnum, pat, j, MAXPAT)
      elseif(arg(i) .eq. STOP_TAG .and. tagnst .gt. 0)then
         junk = addset_(STOP_TAG, pat, j, MAXPAT)
         junk = addset_(tagstk(tagnst), pat, j, MAXPAT)
         tagnst = tagnst - 1
      else
         junk = addset_(CHARA, pat, j, MAXPAT)
         junk = addset_(esc_(arg, i), pat, j, MAXPAT)
      endif
      lastj = lj
      i = i + 1
   enddo MAKPATIT
   if(arg(i) .ne. delim)then
      ! terminated early
      makpat_ = ERR
   elseif(addset_(EOS, pat, j, MAXPAT) .eq. NO)then
      ! no room
      makpat_ = ERR
   elseif(tagnst .ne. 0)then
      makpat_ = ERR
   else
      makpat_ = i
   endif
end function makpat_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    amatch - look for pattern matching regular expression; returns its location
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    loc = amatch(line, from, pat, tagbeg, tagend)
!!
!!     character(len=*),intent(in) :: line
!!     integer,intent(in)          :: from
!!     character                   :: pat(MAXPAT)
!!     integer                     :: tagbeg(10), tagend(10)
!!     integer                     :: loc
!!##DESCRIPTION
!!    AMATCH scans LINE starting at location FROM, looking
!!    for a pattern which matches the regular expression coded
!!    in PAT. If the pattern is found, its starting location
!!    in LINE is returned. If the pattern is not found, AMATCH
!!    returns 0.
!!
!!    The regular expression in PAT must have been previously
!!    encoded by GETPAT(3f) or MAKPAT(3f). (For a complete description
!!    of regular expressions, see the manpage for M_BRE.)
!!
!!    AMATCH(3f) is a special-purpose version of MATCH(3f), which should
!!    be used in most cases.
!!##OPTIONS
!!    LINE           input line to scan
!!    FROM           beginning location to start scan from
!!    PAT            coded regular expression encoded by GETPAT(3f) or MAKPAT(3f)
!!    TAGBEG,TAGEND  element "i + 1" returns start or end, respectively, of "i"th tagged subpattern
!!##RETURNS
!!    LOC   returns location match was found or zero (0) if no match remains
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_amatch
!!     use :: M_BRE, only : getpat, amatch
!!     use :: M_BRE, only : MAXPAT, MAXARG, MAXLINE, MAXTAGS, YES, ERR
!!     implicit none
!!     ! find _ find patterns in text
!!     integer                      :: pat(MAXPAT)
!!     character(len=MAXARG-1)      :: argument
!!     integer                      :: stat
!!     integer                      :: ios
!!     integer                      :: len_arg
!!     integer                      :: loc
!!     integer                      :: ii
!!     character(len=MAXLINE-2)     :: line
!!     integer                      :: tagbeg(MAXTAGS),tagend(MAXTAGS)
!!     call get_command_argument(1, argument,status=stat,length=len_arg)
!!     if(stat.ne.0.or.argument.eq.'')then
!!        write(*,*)"usage: find pattern."
!!     elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
!!        write(*,*)"illegal pattern."
!!     else
!!        INFINITE: do
!!           read(*,'(a)',iostat=ios)line
!!           tagbeg=-9999;tagend=-9999
!!           if(ios.ne.0)exit
!!           loc = amatch(trim(line), 1, pat, tagbeg, tagend) ! returns location/0
!!           if(loc.gt.0)then ! matched; if no match, loc is returned as 0
!!              write(*,'(*(a))')trim(line)
!!              ! (element "i + 1" returns start or end, respectively, of "i"th tagged subpattern)
!!              write(*,'(*(i0,1x,i0,1x,i0,/))')(ii,tagbeg(ii),tagend(ii),ii=1,size(tagbeg))
!!           endif
!!        enddo INFINITE
!!     endif
!!     end program demo_amatch
!!
!!##SEE ALSO
!!    match, getpat, makpat
!!##DIAGNOSTICS
!!    None
!!##AUTHOR
!!    John S. Urban
!!##REFERENCE
!!    "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!    Public Domain
integer function amatch__(line, from, pat, tagbeg, tagend)

! ident_15="@(#)M_BRE::amatch(3f): (non-recursive) look for match starting at lin(from)"

character(len=*),intent(in) :: line
integer                     :: from
integer                     :: pat(MAXPAT)
integer                     :: tagbeg(MAXTAGS), tagend(MAXTAGS)
integer                     :: lin(MAXLINE)
integer                     :: len_line
   len_line=len(line)
   if(len_line.gt.MAXLINE-1)then
      write(*,*)'*amatch* error: input string too long,',len_line,' > ',MAXLINE-2
      stop
   else
      lin(1:len_line+1)=f2r(line) ! convert string to ADE array
      amatch__=amatch_(lin,from,pat,tagbeg,tagend)
   endif
end function amatch__
integer function amatch_(lin, from, pat, tagbeg, tagend)

! ident_16="@(#)M_BRE::amatch(3f): (non-recursive) look for match starting at lin(from)"

integer :: lin(MAXLINE)
integer :: pat(MAXPAT)
integer from, tagbeg(MAXTAGS), tagend(MAXTAGS)
integer i, j, offset, stack
   ! zero out tag data
   tagbeg = 0
   tagend = 0
   tagbeg(1) = from
   stack = 0
   offset = from
   j = 1
   MATCHIT: do
      select case(pat(j))
       case(EOS)
         exit MATCHIT
       case(CLOSURE)
         stack = j
         j = j + CLOSIZE
         i = offset
         OMATCHIT: do
            if(.not.(lin(i) .ne. EOS))exit OMATCHIT
            ! match as many as possible
            if(omatch_(lin, i, pat, j) .eq. NO) exit OMATCHIT
         enddo OMATCHIT
         pat(stack + 1) = i - offset   !! OTHER SAYS COUNT INSTEAD OF 1
         pat(stack + START) = offset
         offset = i  ! character that made us fail
       case(START_TAG)
         i = pat(j + 1)
         tagbeg(i + 1) = offset
       case(STOP_TAG)
         i = pat(j + 1)
         tagend(i + 1) = offset
       case default
         if(omatch_(lin, offset, pat, j) .eq. NO)then
            ! non-closure
            STACKIT: do
               if(stack .le. 0)exit STACKIT
               if(pat(stack + 1) .gt. 0) exit STACKIT  !! OTHER SAYS COUNT INSTEAD OF 1
               stack = pat(stack + 2)  !! OTHER SAYS PREVCL INSTEAD OF 2
            enddo STACKIT
            if(stack .le. 0)then  ! stack is empty
               amatch_ = NO         ! return failure
               return
            endif
            pat(stack + 1) = pat(stack + 1) - 1 !! OTHER SAYS + COUNT INSTEAD OF + 1
            j = stack + CLOSIZE
            offset = pat(stack + START) + pat(stack + 1) !! OTHER SAYS 1==>COUNT
         endif
      end select
      ! else omatch succeeded
      j = j + patsiz_(pat, j)
   enddo MATCHIT
   amatch_ = offset
   tagend(1) = offset
end function amatch_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    omatch(3f) - [M_BRE] try to match a single pattern at pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function omatch(lin, i, pat, j)
!!##DESCRIPTION
!!    try to match a single pattern at pat(j)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
integer function omatch_(lin, i, pat, j)

! ident_17="@(#)M_BRE::omatch(3f): try to match a single pattern at pat(j)"

integer :: lin(MAXLINE)
integer :: pat(MAXPAT)
integer :: i, j, k
integer :: bump
   omatch_ = NO
   if(lin(i) .ne. EOS)then
      bump = -1
      select case(pat(j))
       case(CHARA);  if(lin(i) .eq. pat(j + 1))                   bump = 1
       case(BOL);    if(i .eq. 1)                                 bump = 0
       case(ANY);    if(lin(i) .ne. NEWLINE)                      bump = 1
       case(EOL);    if(lin(i) .eq. NEWLINE .or. lin(i) .eq. EOS) bump = 0
       case(CCL);    if(locate_(lin(i), pat, j + 1) .eq. YES)    bump = 1
       case(NCCL);   if(lin(i).ne. NEWLINE .and. locate_(lin(i), pat, j + 1) .eq. NO) bump = 1
       !! OTHER case(START_TAG)
          !! OTHER k=pat(j+1)
          !! OTHER bpos(k+1) = i
          !! OTHER bump = 0
       !! OTHER case(END_TAG)
          !! OTHER k=pat(j+1)
          !! OTHER epos(k+1) = i
          !! OTHER bump = 0
       case default; call error("in omatch: cannot happen.")
      end select
      if(bump .ge. 0)then
         i = i + bump
         omatch_ = YES
      endif
   endif
end function omatch_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    esc(3f) - [M_BRE] map array(i) into escaped character if appropriate
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer function esc(array, i)
!!    integer,intent(in) :: array(*)
!!    integer            :: i
!!
!!##DESCRIPTION
!!    To support commonly used non-printable characters escaped strings are
!!    supported. When the ESCAPE character is encountered the following
!!    character is examined. If one of the special characters ESC(3f) will
!!    increment I and return the designated non-printable character. Otherwise
!!    it will return the character as-is.
!!
!!    o convert \n to newline
!!    o convert \t to horizontal tab
!!    o convert \r to carriage return
!!    o convert \b to backspace
!!    o convert \nnn to character represented by octal value
!!
!!##OPTIONS
!!    ARRAY  array of ADE (ASCII Decimal Equivalent) values terminated by
!!           an EOS (End-Of-String) character representing a string to scan
!!           for escaped characters.
!!    I      pointer into ARRAY. It is incremented to the position of the
!!           next character in ARRAY on return.
!!
!!##RETURNS
!!    ESC    The ADE for the substituted character
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!
!!##LICENSE
!!   Public Domain
integer function esc_(array, i)

! ident_18="@(#)M_BRE::esc(3f): map array(i) into escaped character if appropriate"

integer,intent(in) :: array(*)
integer            :: i
   if(array(i) .ne. ESCAP)then                 ! if not an escape character, return the character as-is
      esc_ = array(i)
   elseif((array(i+1) .eq. EOS))then           ! if ESCAP is the last character it is left as-is and is not special
      esc_ = ESCAP
   else
      i = i + 1                                 ! increment I to look at character after ESCAP
      select case(array(i))                     ! make substitution to special character for designated characters
       case(CHARN,LETN); esc_ = NEWLINE
       case(CHART,LETT); esc_ = HT
       case(CHARR,LETR); esc_ = CR
       case(CHARB,LETB); esc_ = BACKSPACE
       case(DIG0:DIG7)
         esc_=0
         do while(array(i) >= DIG0 .and. array(i) <= DIG7)
            i = i + 1
            esc_ = 8*esc_ + array(i) - DIG0
            i = i - 1    ! so like other cases
         enddo
       case default;     esc_ = array(i)        ! otherwise just copy character
      end select
   endif
end function esc_
!----------------------------------------------------------------------------------------------------------------------------------!
! Conventional C Constants
!   Oct  Dec  Hex  Char
!   -----------------------
!   000  0    00   NUL '\0'   Null
!   007  7    07   BEL '\a'   Bell
!  *010  8    08   BS  '\b'   Backspace
!  *011  9    09   HT  '\t'   Horizontal Tab
!  *012  10   0A   LF  '\n'   Line Feed
!   013  11   0B   VT  '\v'   Vertical Tab
!   014  12   0C   FF  '\f'   Form Feed
!  *015  13   0D   CR  '\r'   Carriage Return
!   134  92   5C   \   '\\'   Backslash
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
function f2r(string)

! ident_19="@(#)M_BRE::f2r(3f): convert Fortran character variable to Ratfor integer array with Ratfor terminator"

character(len=*),intent(in) :: string
integer                     :: isize
integer                     :: i
integer                     :: f2r(len(string)+1)
   f2r=EOS
   isize=len(string)
   !!GFORTRAN_BUG lin(:len_line)=ichar([(line(i:i),i=1,isize)])
   do i=1,isize
      f2r(i)=ichar(string(i:i))
   enddo
   f2r(i)=EOS
   !!write(*,'(*(g0))')'*f2r* ',i,'['//trim(string)//']'
end function f2r
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
function r2f(ints)

! ident_20="@(#)M_BRE::r2f(3f): convert Ratfor integer array to Fortran character variable"

integer,intent(in)          :: ints(:)
character(len=size(ints)-1) :: r2f
integer                     :: i
intrinsic char
   r2f=' '
   do i=1,size(ints)-1
      if(ints(i).eq.eos)then
         exit
      endif
      r2f(i:i)=char(ints(i))
   enddo
end function r2f
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_suite_M_BRE()
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_verify,   only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
implicit none
call test_regular_expressions()
!===================================================================================================================================
contains
!===================================================================================================================================
subroutine test_regular_expressions()
call unit_check_start('M_BRE')
!    mymatch("regexp",     "String",        expected result )
call mymatch("Foo",        "FooBar",        .true.   )
call mymatch("Poo",        "FooBar",        .false.  )
call mymatch("Bar",        "FooBar",        .true.   )
call mymatch("Par",        "FooBar",        .false.  )
call mymatch("Foo",        "Foo",           .true.   )
call mymatch("Fo",         "Foo",           .true.   )
call mymatch("Foo",        "Fo",            .false.  )
call mymatch("ooB",        "FooBar",        .true.   )
call mymatch("ooP",        "FooBar",        .false.  )
call mymatch(".",          "FooBar",        .true.   )
call mymatch("P.",         "FooBar",        .false.  )
call mymatch("^Foo",       "FooBar",        .true.   )
call mymatch("^Bar",       "FooBar",        .false.  )
call mymatch("Foo$",       "FooBar",        .false.  )
call mymatch("Bar$",       "FooBar",        .true.   )
call mymatch(".*o",        "FooBar",        .true.   )
call mymatch("o*o",        "FooBar",        .true.   )
call mymatch("P*o",        "FooBar",        .true.   )
call mymatch("Fo*o",       "FooBar",        .true.   )
call mymatch("Po*o",       "FooBar",        .false.  )
call mymatch("F[po]o",     "FooBar",        .true.   )
call mymatch("F[op]o",     "FooBar",        .true.   )
call mymatch("F[qp]o",     "FooBar",        .false.  )
call mymatch("F[^po]o",    "FooBar",        .false.  )
call mymatch("F[^op]o",    "FooBar",        .false.  )
call mymatch("F[^qp]o",    "FooBar",        .true.   )
call mymatch("F[po]*o",    "FooBar",        .true.   )
call mymatch("F[56]*o",    "F5oBar",        .true.   )
call mymatch("F[46]*o",    "F5oBar",        .false.  )
call mymatch("F[46]*5",    "F5oBar",        .true.   )
call mymatch("F[46]*5o",   "F5oBar",        .true.   )
call mymatch("F[op]*o",    "FooBar",        .true.   )
call mymatch("F[qp]*o",    "FooBar",        .true.   )
call mymatch("P[qp]*o",    "FooBar",        .false.  )
call mymatch("F[^po]*o",   "FooBar",        .true.   )
call mymatch("F[^op]*o",   "FooBar",        .true.   )
call mymatch("F[^qp]*o",   "FooBar",        .true.   )
call mymatch("P[^qp]*o",   "FooBar",        .false.  )

call mymatch("[0-9][0-9]*$",  "0123456789",  .true.  )
call mymatch("[0-9][0-9]*$",  "A0123456789", .true.  )
call mymatch("^[0-9][0-9]*$", "A0123456789", .false. )
call mymatch("^[0-9][0-9]*$", "",            .false. )
call mymatch("^[0-9]$", "",                  .false. )
call mymatch("^[0-9]*$", "",                 .true.  )
call mymatch("^$", "",                        .true. )
call mymatch("^$", " ",                       .false.)
call mymatch("^[A-Z ][A-Z ]*$", "",          .false. )
call mymatch("^[ ]*[A-Z][A-Z ]*$", " THIS IS ALL UPPERCASE",    .true.   )
call mymatch("^[ ]*[a-z][a-z ]*$", " this is all lowercase",    .true.   )
call mymatch("^[ ]*[A-Z][A-Z ]*$", " THIS IS not ALL UPPERCASE",    .false.  )
call mymatch("^[ ]*[a-z][a-z ]*$", " this is NOT all lowercase",    .false.  )

! check dash in character class at beginning and end instead of in range
call mymatch("X[-+]Y", "X-Y",                        .true. )
call mymatch("X[-+]Y", "X+Y",                        .true. )
call mymatch("X[+-]Y", "X-Y",                        .true. )
call mymatch("X[-+]Y", "Y-X",                        .false. )
call mymatch("X[-+]Y", "Y+X",                        .false. )
call mymatch("X[+-]Y", "Y-X",                        .false. )
call mymatch("X[+-]Y", "Y+X",                        .false. )
call mymatch("X[+-]Y", "X+Y",                        .true. )
! tabs
call mymatch("X\tY", "X"//char(HT)//"Y",             .true. )
call mymatch("X[\tab]Y", "X"//char(HT)//"Y",         .true. )
call mymatch("X[\tab]Y", "XtY",                      .false. )
call mymatch("X[\tab]Y", "XaY",                      .true. )

call mymatch("[0-9][0-9]*\.[0-9]*",   "1.9",           .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.99",          .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.999",         .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.9999",        .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.99999",       .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "11.99999",      .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "111.99999",     .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1111.99999",    .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "11111.99999",   .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "123456.99999",  .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.9",           .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.99",          .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.999",         .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.9999",        .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.99999",       .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "11.99999",      .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "111.99999",     .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1111.99999",    .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "11111.99999",   .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "111111.99999",  .true.   )
call mymatch("a[0-9][0-9]*\.[0-9]*",  "a1.9",          .true.   )
call mymatch("a[0-9][0-9]*\.",        "a1.9",          .true.   )
call mymatch("a[0-9][0-9]*",          "a1.9",          .true.   )
call mymatch("a",                "a1.9",          .true.   )
call mymatch("\\",               "\",             .true.   )
call mymatch("\.",               "\",             .false.  )
call mymatch(".",                "\",             .true.   )
call mymatch("F[qpo", "FooBar", .false.) ! intentional bad regex

call unit_check_done('M_BRE')
end subroutine test_regular_expressions
!===================================================================================================================================
subroutine mymatch(expression,string,expected)
character(len=*),intent(in) :: expression
character(len=*),intent(in) :: string
character(len=MAXARG)       :: expression_local
character(len=MAXLINE)      :: string_local
logical,intent(in)          :: expected
integer                     :: ii
integer                     :: jj
integer                     :: k
logical                     :: answer
integer                     :: arg(MAXARG)
integer                     :: lin(MAXLINE)
integer                     :: pat(MAXPAT)

   expression_local=expression
   string_local=string
   arg=ichar([(expression_local(k:k),k=1,MAXARG)])
   ii=len_trim(expression_local)+1
   ii=min(ii,MAXLINE)
   arg(ii:ii)=EOS
   jj=getpat_(arg, pat)
   if(jj .eq. ERR) then
      call unit_check('M_BRE',.not.expected,&
      &'illegal pattern for regex',expression,'and string',trim(string),'expected',expected,'getpat=',jj)
   else
      lin=ichar([(string_local(k:k),k=1,MAXLINE)])
      ii=len_trim(string_local)+1
      ii=min(ii,MAXLINE-2)
      lin(ii:ii)=NEWLINE
      ii=ii+1
      lin(ii:ii)=EOS
      answer=match_(lin,pat).eq.YES
      call unit_check('M_BRE',answer .eqv. expected,&
      &'for regex',expression,'and string',trim(string),'expected',expected,'got',answer,'getpat=',jj)
   endif

end subroutine mymatch
!===================================================================================================================================
end subroutine test_suite_M_BRE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_BRE
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

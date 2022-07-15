module M_stringtonumber
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

use iso_fortran_env, only: real32, real64
use iso_fortran_env, only: int8, int16, int32, int64

use iso_c_binding, only : c_double, c_null_char, c_char
use iso_fortran_env, only: wp => real64, ip => int64
private
public  :: ato
interface ato
module procedure ator_real32
module procedure ator_real64
module procedure atoi_int8
module procedure atoi_int16
module procedure atoi_int32
module procedure atoi_int64
end interface
! C interfaces
public :: strtod
public :: atoi_8, atoi_16, atoi_32, atoi_64

contains
logical function ator_real32(str,val,msg)
use iso_fortran_env, only: wp => real32, ip => int64, byte => int8
implicit none
! Convert ASCII-text to DP and return .TRUE. if OK
character(len=*),intent(in) :: str
real(kind=wp) :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=byte),parameter  :: upper_e=iachar('E'), lower_e=iachar('e'), upper_d=iachar('D'), lower_d=iachar('d')
integer(kind=byte),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-'), decimal=iachar('.')
integer(kind=byte),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: sval(3)
integer                       :: digit_count(3)
integer(kind=byte)            :: value(3,len(str))
real(kind=wp)                 :: whole, fractional
integer                       :: power
integer                       :: cnt(6)
integer(kind=byte)            :: a, part
integer                       :: i, ipos, ios, too_many_digit_count

   value=0.0_wp
   cnt=0
   digit_count=0
   ipos=0
   ator_real32 = .false.
   sval = [1,0,1]
   part = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=byte)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         digit_count(part) = digit_count(part) + 1
         if(digit_count(part).lt.19)then
            value(part,digit_count(part)) = a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
      case(decimal)                              ! if more than once should report error
         if(part.gt.2)cnt(5)=99999               ! decimal in exponent
         part = 2                                ! starting fractional value
         cnt(1)=cnt(1)+1
      case(upper_e,lower_e,upper_d,lower_d)      ! if more than once should report error
         part = 3
         cnt(2)=cnt(2)+1                         ! if more than one encountered an error
         ipos=0
      case(minus_sign)                           ! sign in non-standard position or duplicated should report error
         sval(part) = -1
         if(ipos.ne.1)cnt(6)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos.ne.1)cnt(4)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(space)                                ! should possibly not ignore all internal spaces
         ipos=ipos-1
      case default
         value(part,:) = 0.0_wp
         cnt(5)=99999                            ! unknown character
         !return
      end select
   enddo
   ! is no value after E an error?
   whole=0.0_wp
   do i = digit_count(1),1,-1
      whole=whole+value(1,i)*10**(digit_count(1)-i)
   enddo

   power=0
   do i = digit_count(3),1,-1
      power=power+value(3,i)*10**(digit_count(3)-i)
   enddo

   fractional=0.0_wp
   do i = digit_count(2),1,-1
      fractional=fractional+real(value(2,i),kind=wp)/10.0_wp**i
   enddo

   associate ( sgn=>sval(1), sexp=>sval(3) )
   val = sign(whole + fractional,real(sgn,kind=wp))* (10.0_wp**(power*sexp+too_many_digit_count))
   end associate
   if(all(cnt.le.1).and.ipos.ne.0)then
      ator_real32 = .true.
   else
      read(str,fmt=*,iostat=ios) val ! use internal read for INF, NAN for now
      if(ios.eq.0)then
         ator_real32 = .true.
      else
         if(present(msg))then
            if(cnt(5).ne.0)then
                  msg='illegal character in value "'//trim(str)//'"'
               elseif(cnt(5).ne.0)then
                  msg='decimal in exponent in "'//trim(str)//'"'
               elseif(cnt(1).ge.2)then
                  msg='multiple decimals in "'//trim(str)//'"'
               elseif(cnt(2).ge.2)then
                  msg='more than one exponent prefix (e,d,E,D) in "'//trim(str)//'"'
               elseif(cnt(3).ge.2)then
                  msg='more than one sign character in "'//trim(str)//'"'
               elseif(cnt(6).ne.0)then
                  msg='- sign character not first in "'//trim(str)//'"'
               elseif(cnt(4).ge.2)then
                  msg='+ sign character not first in "'//trim(str)//'"'
               else
                  msg='error in data conversion in "'//trim(str)//'"'
               endif
         endif
         ator_real32 = .false.
      endif
   endif
end function ator_real32
logical function ator_real64(str,val,msg)
use iso_fortran_env, only: wp => real64, ip => int64, byte => int8
implicit none
! Convert ASCII-text to DP and return .TRUE. if OK
character(len=*),intent(in) :: str
real(kind=wp) :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=byte),parameter  :: upper_e=iachar('E'), lower_e=iachar('e'), upper_d=iachar('D'), lower_d=iachar('d')
integer(kind=byte),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-'), decimal=iachar('.')
integer(kind=byte),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: sval(3)
integer                       :: digit_count(3)
integer(kind=byte)            :: value(3,len(str))
real(kind=wp)                 :: whole, fractional
integer                       :: power
integer                       :: cnt(6)
integer(kind=byte)            :: a, part
integer                       :: i, ipos, ios, too_many_digit_count

   value=0.0_wp
   cnt=0
   digit_count=0
   ipos=0
   ator_real64 = .false.
   sval = [1,0,1]
   part = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=byte)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         digit_count(part) = digit_count(part) + 1
         if(digit_count(part).lt.19)then
            value(part,digit_count(part)) = a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
      case(decimal)                              ! if more than once should report error
         if(part.gt.2)cnt(5)=99999               ! decimal in exponent
         part = 2                                ! starting fractional value
         cnt(1)=cnt(1)+1
      case(upper_e,lower_e,upper_d,lower_d)      ! if more than once should report error
         part = 3
         cnt(2)=cnt(2)+1                         ! if more than one encountered an error
         ipos=0
      case(minus_sign)                           ! sign in non-standard position or duplicated should report error
         sval(part) = -1
         if(ipos.ne.1)cnt(6)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos.ne.1)cnt(4)=99999               ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                         ! if more than one sign character an error, but caught by not being first
      case(space)                                ! should possibly not ignore all internal spaces
         ipos=ipos-1
      case default
         value(part,:) = 0.0_wp
         cnt(5)=99999                            ! unknown character
         !return
      end select
   enddo
   ! is no value after E an error?
   whole=0.0_wp
   do i = digit_count(1),1,-1
      whole=whole+value(1,i)*10**(digit_count(1)-i)
   enddo

   power=0
   do i = digit_count(3),1,-1
      power=power+value(3,i)*10**(digit_count(3)-i)
   enddo

   fractional=0.0_wp
   do i = digit_count(2),1,-1
      fractional=fractional+real(value(2,i),kind=wp)/10.0_wp**i
   enddo

   associate ( sgn=>sval(1), sexp=>sval(3) )
   val = sign(whole + fractional,real(sgn,kind=wp))* (10.0_wp**(power*sexp+too_many_digit_count))
   end associate
   if(all(cnt.le.1).and.ipos.ne.0)then
      ator_real64 = .true.
   else
      read(str,fmt=*,iostat=ios) val ! use internal read for INF, NAN for now
      if(ios.eq.0)then
         ator_real64 = .true.
      else
         if(present(msg))then
            if(cnt(5).ne.0)then
                  msg='illegal character in value "'//trim(str)//'"'
               elseif(cnt(5).ne.0)then
                  msg='decimal in exponent in "'//trim(str)//'"'
               elseif(cnt(1).ge.2)then
                  msg='multiple decimals in "'//trim(str)//'"'
               elseif(cnt(2).ge.2)then
                  msg='more than one exponent prefix (e,d,E,D) in "'//trim(str)//'"'
               elseif(cnt(3).ge.2)then
                  msg='more than one sign character in "'//trim(str)//'"'
               elseif(cnt(6).ne.0)then
                  msg='- sign character not first in "'//trim(str)//'"'
               elseif(cnt(4).ge.2)then
                  msg='+ sign character not first in "'//trim(str)//'"'
               else
                  msg='error in data conversion in "'//trim(str)//'"'
               endif
         endif
         ator_real64 = .false.
      endif
   endif
end function ator_real64
logical function atoi_int8(str,val,msg)
use iso_fortran_env, only: ip => int64, byte => int8
implicit none
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int8)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=byte),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=byte),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=byte)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=byte)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count.lt.19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0).lt.huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos.ne.1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos.ne.1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt.le.1).and.ipos.ne.0)then
      atoi_int8 = .true.
   else
      if(present(msg))then
         if(cnt(5).ne.0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3).ge.2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6).ne.0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4).ge.2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int8 = .false.
   endif
end function atoi_int8
logical function atoi_int16(str,val,msg)
use iso_fortran_env, only: ip => int64, byte => int8
implicit none
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int16)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=byte),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=byte),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=byte)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=byte)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count.lt.19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0).lt.huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos.ne.1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos.ne.1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt.le.1).and.ipos.ne.0)then
      atoi_int16 = .true.
   else
      if(present(msg))then
         if(cnt(5).ne.0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3).ge.2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6).ne.0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4).ge.2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int16 = .false.
   endif
end function atoi_int16
logical function atoi_int32(str,val,msg)
use iso_fortran_env, only: ip => int64, byte => int8
implicit none
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int32)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=byte),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=byte),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=byte)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=byte)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count.lt.19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0).lt.huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos.ne.1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos.ne.1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt.le.1).and.ipos.ne.0)then
      atoi_int32 = .true.
   else
      if(present(msg))then
         if(cnt(5).ne.0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3).ge.2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6).ne.0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4).ge.2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int32 = .false.
   endif
end function atoi_int32
logical function atoi_int64(str,val,msg)
use iso_fortran_env, only: ip => int64, byte => int8
implicit none
! Convert ASCII-text to REAL and return .TRUE. if OK
character(len=*),intent(in)   :: str
integer(kind=int64)         :: val
character(len=:),allocatable,optional,intent(out) :: msg
integer(kind=byte),parameter  :: plus_sign=iachar('+'), minus_sign=iachar('-')
integer(kind=byte),parameter  :: space=iachar(' '), digit_0=iachar('0'), digit_9=iachar('9')
integer(kind=ip)              :: value, sval, digit_count
integer                       :: cnt(6)
integer(kind=byte)            :: a
integer                       :: i, ipos, too_many_digit_count

   value=0
   cnt=0
   digit_count=0
   ipos=0
   sval = 1
   too_many_digit_count=0
   do i = 1, len(str)
      a=iachar(str(i:i),kind=byte)
      ipos=ipos+1
      select case(a)
      case(digit_0:digit_9)
         if(digit_count.lt.19)then
            value = value*10 + a-digit_0
         elseif(real(value*10)+real(a-digit_0).lt.huge(0_ip))then
            value = value*10 + a-digit_0
         else
            too_many_digit_count=too_many_digit_count+1    ! so many digit_count just use powers of ten after this
         endif
         digit_count = digit_count + 1
      case(minus_sign)                         ! sign in non-standard position or duplicated should report error
         sval = -1
         if(ipos.ne.1)cnt(6)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(plus_sign)
         if(ipos.ne.1)cnt(4)=99999             ! sign not first character of whole or exponent part
         cnt(3)=cnt(3)+1                       ! if more than one sign character an error, but caught by not being first
      case(space)                              ! should possibly not ignore all internal spaces (and maybe ignore commas too?)
         ipos=ipos-1
      case default
         value = 0
         cnt(5)=99999                          ! unknown character
      end select
   enddo
   val = sign(value,sval)* 10**too_many_digit_count
   if(all(cnt.le.1).and.ipos.ne.0)then
      atoi_int64 = .true.
   else
      if(present(msg))then
         if(cnt(5).ne.0)then
               msg='illegal character in value "'//trim(str)//'"'
            elseif(cnt(3).ge.2)then
               msg='more than one sign character in "'//trim(str)//'"'
            elseif(cnt(6).ne.0)then
               msg='- sign character not first in "'//trim(str)//'"'
            elseif(cnt(4).ge.2)then
               msg='+ sign character not first in "'//trim(str)//'"'
            else
               msg='error in data conversion in "'//trim(str)//'"'
            endif
      endif
      atoi_int64 = .false.
   endif
end function atoi_int64
function strtod(str)
implicit none
real(kind=wp) :: strtod
!$@(#) use C strtod
character(len=*),intent(in) :: str
character(len=1,kind=c_char),allocatable :: c_str(:)
interface
   function c_strtod(c_string,final) bind(c,name="strtod")
   use iso_c_binding
   character(kind=c_char) :: c_string(*)
   type(c_ptr),optional :: final
   real(c_double) c_strtod
   end function c_strtod
end interface
   c_str=str2_carr(str)
   strtod=c_strtod(c_str)
end function strtod

pure function str2_carr(string) result (array)

!$@(#) M_system::str2_carr(3fp): function copies string to null terminated char array

character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)
integer                      :: i
   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(i:i)=c_null_char
end function str2_carr
function atoi_8(str)
implicit none
integer(kind=int8) :: atoi_8
!$@(#) use C strtod
character(len=*),intent(in) :: str
character(len=1,kind=c_char),allocatable :: c_str(:)
interface
   function c_atoi(string) bind(c,name="atoi")
   use iso_c_binding
   character(kind=c_char) string(*)
   integer(c_short) c_atoi
   end function c_atoi
end interface
   c_str=str2_carr(str)
   atoi_8=c_atoi(c_str)
end function atoi_8

function atoi_16(str)
implicit none
integer(kind=int16) :: atoi_16
!$@(#) use C strtod
character(len=*),intent(in) :: str
character(len=1,kind=c_char),allocatable :: c_str(:)
interface
   function c_atoi(string) bind(c,name="atoi")
   use iso_c_binding
   character(kind=c_char) string(*)
   integer(c_short) c_atoi
   end function c_atoi
end interface
   c_str=str2_carr(str)
   atoi_16=c_atoi(c_str)
end function atoi_16

function atoi_32(str)
implicit none
integer(kind=int32) :: atoi_32
!$@(#) use C strtod
character(len=*),intent(in) :: str
character(len=1,kind=c_char),allocatable :: c_str(:)
interface
   function c_atoi(string) bind(c,name="atoi")
   use iso_c_binding
   character(kind=c_char) string(*)
   integer(c_int) c_atoi
   end function c_atoi
end interface
   c_str=str2_carr(str)
   atoi_32=c_atoi(c_str)
end function atoi_32

function atoi_64(str)
implicit none
integer(kind=int64) :: atoi_64
!$@(#) use C strtod
character(len=*),intent(in) :: str
character(len=1,kind=c_char),allocatable :: c_str(:)
interface
   function c_atoi(string) bind(c,name="atol")
   use iso_c_binding
   character(kind=c_char) string(*)
   integer(c_long) c_atoi
   end function c_atoi
end interface
   c_str=str2_carr(str)
   atoi_64=c_atoi(c_str)
end function atoi_64
end module M_stringtonumber

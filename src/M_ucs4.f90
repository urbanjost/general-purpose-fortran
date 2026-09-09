module M_ucs4
use iso_fortran_env, only: error_unit, stderr=>error_unit
! Unicode-related procedures requiring Fortran support of ISO-10646.
! first presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
! including enhancements and latin support from Francois Jacq, 2025-08
!
implicit none

private
public :: utf8_to_ucs4,            ucs4_to_utf8
public :: utf8_to_ucs4_via_io,     ucs4_to_utf8_via_io
public :: ascii_to_ucs4,           ucs4_to_ascii
public :: extended_ascii_to_ucs4,  ucs4_to_extended_ascii
public :: utf8_to_codepoints,      codepoints_to_utf8

private :: a2s, s2a

interface utf8_to_codepoints
   module procedure utf8_to_codepoints_str, utf8_to_codepoints_chars
end interface utf8_to_codepoints

interface codepoints_to_utf8
   module procedure codepoints_to_utf8_str, codepoints_to_utf8_chars
end interface codepoints_to_utf8

integer,parameter :: ucs4=selected_char_kind('ISO_10646') ! The compiler must support UCS-4 characters
integer,parameter :: ascii=selected_char_kind('ascii') ! maybe should use default, as ASCII is technically 128, not 256 chars
integer,parameter :: default=selected_char_kind('default') 

contains

function utf8_to_ucs4(string) result(corrected)
! return a string of kind ucs4 from bytes representing utf8 glyphs
character(len=*),intent(in)             :: string
character(len=:,kind=ucs4),allocatable  :: corrected
integer,allocatable                     :: codepoints(:)
integer                                 :: i, n
integer                                 :: err
   call utf8_to_codepoints(string,codepoints,err)
   n=size(codepoints)
   allocate(character(len=n,kind=ucs4)  :: corrected)

   if(.not.allocated(codepoints))then
      corrected=''
      return
   endif
   do i=1,n
      corrected(i:i)=char(codepoints(i),kind=ucs4)
   enddo
end function utf8_to_ucs4

function ucs4_to_utf8(ucs4_string,err) result(string)
! return bytes representing utf8 glyphs from a string of kind ucs4
character(len=*,kind=ucs4),intent(in)   :: ucs4_string  ! stores the UCS-4 string.
integer, intent(out),optional           :: err
character(len=:),allocatable            :: string
integer                                 :: codepoints(len(ucs4_string))
integer                                 :: i
integer                                 :: nerr

   codepoints=[(ichar(ucs4_string(i:i)),i=1,len(ucs4_string))]
   call codepoints_to_utf8(codepoints,string,nerr)
   if(present(err))then
      err=nerr
   elseif(nerr.ne.0)then
      stop '<ERROR>*ucs4_to_utf8*'
   endif

end function ucs4_to_utf8

function utf8_to_ucs4_via_io(string) result(corrected)
character(len=*),intent(in)            :: string
character(len=:,kind=ucs4),allocatable :: corrected
character(len=(len(string)),kind=ucs4) :: line
character(len=255)                     :: iomsg
integer                                :: i
integer                                :: lun
integer                                :: iostat
   open(newunit=lun,encoding='UTF-8',status='scratch')
   do i=1,len(string)
      write(lun,'(A)',iostat=iostat,iomsg=iomsg,advance='no')string(i:i)
      if(iostat.ne.0)then
         ! not definite: after an error the position may be undefined
         write(lun,'(A)',iostat=iostat,iomsg=iomsg,advance='no')'?'
         write(stderr,'(A)')trim(iomsg)
      endif
   enddo
   write(lun,'(A)',advance='yes')
   rewind(lun)
   read(lun,'(A)',iostat=iostat)line
   close(lun)
   corrected=trim(line)
end function utf8_to_ucs4_via_io

function ucs4_to_utf8_via_io(ucs4_string) result(corrected)
character(len=*,kind=ucs4),intent(in)          :: ucs4_string
character(len=:,kind=ascii),allocatable        :: corrected
character(len=(len(ucs4_string)*4),kind=ascii) :: line
integer                                        :: lun
   open(newunit=lun,encoding='UTF-8',status='scratch')
   write(lun,'(A)')ucs4_string
   rewind(lun)
   open(unit=lun,encoding='default')
   read(lun,'(A)')line
   close(lun)
   corrected=trim(line)
end function ucs4_to_utf8_via_io

function ascii_to_ucs4(astr) result(ustr)
! @(#) make the same conversion as an assignment statement from ASCII to UCS4
character(len=*,kind=ascii),intent(in) :: astr
character(len=len(astr),kind=ucs4)     :: ustr
integer                                :: i
   do i=1,len(astr)
      ustr(i:i)=achar(iachar(astr(i:i)),kind=ucs4)
   enddo
end function ascii_to_ucs4

function ucs4_to_ascii(ustr) result(astr)
! @(#) make the same conversion as an assignment statement from UCS4 to ASCII
character(len=*,kind=ucs4),intent(in)  :: ustr
character(len=len(ustr),kind=ascii)    :: astr
integer                                :: i
   do i=1,len(ustr)
      astr(i:i)=achar(iachar(ustr(i:i)),kind=ascii)
   enddo
end function ucs4_to_ascii

function extended_ascii_to_ucs4(astr) result(ustr)
! @(#) make the conversion extended_ascii to UCS4
character(len=*,kind=default),intent(in) :: astr
character(len=len(astr),kind=ucs4)       :: ustr
integer                                  :: i
   do i=1,len(astr)
      ustr(i:i)=char(ichar(astr(i:i)),kind=ucs4)
   enddo
end function extended_ascii_to_ucs4

function ucs4_to_extended_ascii(ustr) result(astr)
! @(#) make the conversion from UCS4 to extended_ascii
character(len=*,kind=ucs4),intent(in)  :: ustr
character(len=len(ustr),kind=default)  :: astr
integer                                :: i
   do i=1,len(ustr)
      astr(i:i)=char(ichar(ustr(i:i)),kind=default)
   enddo
end function ucs4_to_extended_ascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_chars(unicode,utf8,nerr)

integer,intent(in)                :: unicode(:)
character,allocatable,intent(out) :: utf8(:)
integer,intent(out)               :: nerr
character(len=256)                :: nerrmsg
integer                           :: i, n_unicode, n_utf8, cp
character, allocatable            :: temp_utf8(:)

   n_unicode = size(unicode)

   allocate(temp_utf8(4*n_unicode))
   n_utf8 = 0

   nerr=0
   do i = 1, n_unicode
      cp = unicode(i)

      select case (cp)
      case (0:127) ! 1 byte : 0xxxxxxx
         n_utf8 = n_utf8 + 1
         temp_utf8(n_utf8) = char(cp)

      case (128:2047) ! 2 bytes : 110xxxxx 10xxxxxx
         n_utf8 = n_utf8 + 2
         temp_utf8(n_utf8-1) = char(ior(192, ishft(cp, -6)))
         temp_utf8(n_utf8)   = char(ior(128, iand(cp, 63)))

      case (2048:65535) ! 3 bytes : 1110xxxx 10xxxxxx 10xxxxxx
         if (cp >= 55296 .and. cp <= 57343) then
            nerr=nerr+1
            n_utf8 = n_utf8 + 1
            temp_utf8(n_utf8) = '?'
            cycle
         endif
         n_utf8 = n_utf8 + 3
         temp_utf8(n_utf8-2) = char(ior(224, ishft(cp, -12)))
         temp_utf8(n_utf8-1) = char(ior(128, iand(ishft(cp, -6), 63)))
         temp_utf8(n_utf8)   = char(ior(128, iand(cp, 63)))

      case (65536:1114111) ! 4 bytes : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
         n_utf8 = n_utf8 + 4
         temp_utf8(n_utf8-3) = char(ior(240, ishft(cp, -18)))
         temp_utf8(n_utf8-2) = char(ior(128, iand(ishft(cp, -12), 63)))
         temp_utf8(n_utf8-1) = char(ior(128, iand(ishft(cp, -6), 63)))
         temp_utf8(n_utf8)   = char(ior(128, iand(cp, 63)))

      case default
         nerr=nerr+1
         n_utf8 = n_utf8 + 1
         temp_utf8(n_utf8) = '?'
      end select
   enddo

   allocate(utf8(n_utf8))
   utf8 = temp_utf8(1:n_utf8)

end subroutine codepoints_to_utf8_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_chars(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character            ,intent(in)  :: utf8(:)
integer  ,allocatable,intent(out) :: unicode(:)
integer,intent(out)               :: nerr
integer                           :: n_out
integer                           :: i, len8, b1, b2, b3, b4
integer                           :: cp, nbytes,nerr0
integer,allocatable               :: temp(:)

   nerr = 0

   len8 = size(utf8)
   i = 1
   n_out = 0
   allocate(temp(len8)) ! big enough to store all unicode values

   do while (i <= len8)

      nerr0=nerr

      b1 = ichar(utf8(i))
      if (b1 < 0) b1 = b1 + 256

      nbytes = 1

      select case (b1)

      case (0:127)
         cp = b1

      case (192:223)
         if (i+1 > len8) then
            nbytes=len8-i+1
            nerr = nerr+1
            cp=ICHAR('?')
         else
            nbytes=2
            b2 = ichar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            if (iand(b2, 192) /= 128) then
               nerr=nerr+1
               cp=ICHAR('?')
            else
               cp = iand(b1, 31)
               cp = ishft(cp,6) + iand(b2,63)
            endif
         endif

      case (224:239)
         if (i+2 > len8) then
            nbytes=len8-i+1
            nerr=nerr+1
            cp=ICHAR('?')
         else
            nbytes = 3
            b2 = ichar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = ichar(utf8(i+2)); if (b3 < 0) b3 = b3 + 256
            if (iand(b2, 192) /= 128 .or. iand(b3, 192) /= 128) then
               nerr =nerr+1
               cp=ICHAR('?')
            else
               cp = iand(b1, 15)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
            endif
         endif

      case (240:247)
         if (i+3 > len8) then
            nbytes=len8-i+1
            nerr = nerr+1
            cp=ICHAR('?')
         else
            nbytes = 4
            b2 = ichar(utf8(i+1)); if (b2 < 0) b2 = b2 + 256
            b3 = ichar(utf8(i+2)); if (b3 < 0) b3 = b3 + 256
            b4 = ichar(utf8(i+3)); if (b4 < 0) b4 = b4 + 256
            if (iand(b2,192)/=128 .or. iand(b3,192)/=128 .or. iand(b4,192)/=128) then
               nerr = nerr+1
               cp=ICHAR('?')
            else
               cp = iand(b1, 7)
               cp = ishft(cp,6) + iand(b2,63)
               cp = ishft(cp,6) + iand(b3,63)
               cp = ishft(cp,6) + iand(b4,63)
            endif
         endif

      case default
         nerr=nerr+1
         cp=ICHAR('?')

      end select

      if(nerr0 /= nerr) then
         ! This is an invalid UTF-8 start byte. We apply the heuristic
         ! and interpret it as an ISO-8859-15 character.
         select case (b1)
         case (164); cp = 8364 ! Euro
         case (166); cp = 352  ! S caron
         case (168); cp = 353  ! s caron
         case (180); cp = 381  ! Z caron
         case (184); cp = 382  ! z caron
         case (188); cp = 338  ! OE
         case (189); cp = 339  ! oe
         case (190); cp = 376  ! Y trema
         case default
            cp = b1 ! For all other chars, the codepoint is the byte value
         end select
         nbytes=1
      endif

      n_out = n_out + 1
      temp(n_out) = cp
      i = i + nbytes

   enddo

   allocate(unicode(n_out))
   unicode = temp(1:n_out)

end subroutine utf8_to_codepoints_chars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array)  result (string)

! @(#) M_strs a2s(3fp) function to copy char array to string

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i

   forall( i = 1:size(array)) string(i:i) = array(i)
!  string=transfer(array,string)

end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

! @(#) M_strs s2a(3fp) function to copy string(1 Clen(string)) to char array

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i

   forall(i=1:len(string)) array(i) = string(i:i)
!  array=transfer(string,array)

end function s2a
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine codepoints_to_utf8_str(unicode,utf8,nerr)

integer,intent(in)                       :: unicode(:)
character(len=:),allocatable,intent(out) :: utf8
integer,intent(out)                      :: nerr
character, allocatable                   :: utf8_chars(:)
   nerr=0
   call codepoints_to_utf8_chars(unicode,utf8_chars,nerr)
   utf8=a2s(utf8_chars)
end subroutine codepoints_to_utf8_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_codepoints_str(utf8,unicode,nerr)

! in fact, this routine is also able to decode an ISOLATIN string

character(len=*),intent(in)     :: utf8
integer,allocatable,intent(out) :: unicode(:)
integer,intent(out)             :: nerr
character,allocatable           :: temp(:)
   temp=s2a(utf8)
   call utf8_to_codepoints_chars(temp,unicode,nerr)
end subroutine utf8_to_codepoints_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

end module M_ucs4

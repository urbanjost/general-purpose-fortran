program demo_base64
use,intrinsic :: iso_fortran_env, only : int8, int32, stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT
use  M_io,      only : filebyte, putchar
use  M_strings, only : isspace
use  M_CLI2,    only : set_args, iget, lget, infiles=>unnamed
! encode data to base64 encryption as defined by RFC-4648 and print to standard output
! usage: base64 inputfile > outputfile
! currently stdin and stdout cannot be defined as streams, so stdin should
! be restricted to ASCII files with a newline terminator at end, and stdout is
! potentially subject to linelength limits of the platform.
! reading input into memory could be expensive if the file is large
implicit none
integer(kind=int32)          :: i, j, column, sz, pad, iostat
character(len=1),allocatable :: text(:) ! array to hold file in memory
character(len=1)             :: chunk(4)
character(len=1)             :: tri(3)
character(len=1),allocatable :: trilast(:)
integer                      :: wrap
character(len=*),parameter   :: rfc4648_alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
integer,parameter            :: rfc4648_linelength=76
character(len=1),parameter   :: rfc4648_padding='='
character(len=:),allocatable :: help_text(:), version_text(:)
logical                      :: decode
logical                      :: ignore_garbage
   call setup()
   call set_args('--wrap:w 76 --decode:d F --ignore-garbage:i F',help_text,version_text)

   wrap=iget('wrap')
   ignore_garbage=lget('ignore-garbage')
   decode=lget('decode')
   if(size(infiles).eq.0)infiles=[character(len=1):: '-']
   ! reading the file into memory can be a problem when the files are large, and this routine currently reads stdin
   ! until an end-of-file while writing it to a scratch file first, which can be slow and use file space resources
   ! can change it to process the file in a buffered mannner so reading from stdin is performed more efficiently
   call filebyte(infiles(1),text) ! allocate character array and copy file into it and pad with two characters at end
   if(.not.allocated(text))then
      stop '<ERROR>*base64* text not allocated'
   endif
   select case(decode)
   case(.false.)
      sz=size(text)
      ! potentially an expensive operation
      text=[text,char(0),char(0)]
      pad=3-mod(sz,3)
      column=0
      ! place three bytes and zero into 32bit integer
      ! take sets of 6 bits from integer and place into every 8 bits
      do i=1,sz,3
         chunk=three2four(text(i:i+2))
         if(i.gt.sz-3)then
            if(pad.gt.0.and.pad.lt.3)then
               chunk(5-pad:)=[(rfc4648_padding,j=1,pad)]
            endif
         endif
         do j=1,4
            iostat=putchar(chunk(j))
            column=column+1
           if(wrap.gt.0)then
               if(column.ge.wrap)then
                  iostat=putchar(new_line('a'))
                  column=0
               endif
            endif
         enddo
      enddo
      if(column.ne.0.and.wrap.gt.0) iostat=putchar(new_line('a'))
   case(.true.)
      if(ignore_garbage)then
         text=pack(text,index(rfc4648_alphabet,text).ne.0)
      else
         text=pack(text,.not.isspace(text))
      endif
      do i=1,size(text)-7,4
         tri=four2three(text(i:i+3))
         do j=1,3
            iostat=putchar(tri(j))
         enddo
      enddo
      ! the last four may represent 1,2, or 3 characters depending on the padding of the last quad of input characters
      trilast=four2three(text(i:i+3))
      do j=1,size(trilast)
         iostat=putchar(trilast(j))
      enddo
   end select

contains

function four2three(quad) result(tri)
! place lower 6 bits of four bytes into 32bit integer
! take three sets of 8 bits from integer and place into three bytes
character(len=1),intent(in)  :: quad(4)
character(len=1)             :: out(4)
character(len=1),allocatable :: tri(:)
integer(kind=int32)          :: i32, o32, i
integer(kind=int8)           :: iquad(4)
character(len=1),parameter   :: rfc4648_padding='='
   do i=1,4
      iquad(4-i+1)=index(rfc4648_alphabet,quad(i),kind=int8)-1_int8
   enddo
   i32 = transfer(iquad, i32 )
   ! The bits are numbered 0 to BIT_SIZE(I)-1, from right to left.
   o32=0
   call  mvbits(i32, 0,  6, o32, 0)
   call  mvbits(i32, 8,  6, o32, 6)
   call  mvbits(i32, 16, 6, o32, 12)
   call  mvbits(i32, 24, 6, o32, 18)
   out=transfer(o32, out)

   if(quad(3).eq.rfc4648_padding)then
      tri=out(3:3)
   elseif(quad(4).eq.rfc4648_padding)then
      tri=out(3:2:-1)
   else
      tri=out(3:1:-1)
   endif
end function four2three

function three2four(tri) result(quad)
character(len=1),intent(in) :: tri(3)
character(len=1)            :: quad(4)
integer(kind=int32)         :: i32, j, k, m, iout(4)
   i32 = transfer([(tri(j),j=3,1,-1),achar(0)], i32 )
   iout = 0
   ! The bits are numbered 0 to BIT_SIZE(I)-1, from right to left.
   do j=0,3
      k=4-j
      call  mvbits(i32, (j)*6, 6, iout(k:k), 0)
      m=iout(k)+1
      quad(k)=rfc4648_alphabet(m:m)
   enddo
end function three2four

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   base64-(1f) - [FUNIX:FILESYSTEM] encode/data specified file to stdout',&
'   using base64 encoding as defined in RFC-4648                         ',&
'   (LICENSE:MIT)                                                        ',&
'                                                                        ',&
'SYNOPSIS                                                                ',&
'    base64- [[ --decode] [ --ignore-garbage]]|[ --wrap COLS] [FILE]     ',&
'    |[ --help|--version]                                                ',&
'                                                                        ',&
'DESCRIPTION                                                             ',&
'   base64-(1f) encodes or decodes a a single file onto standard output. ',&
'                                                                        ',&
'   The data is encoded as described for the base64-alphabet-encoding in ',&
'   RFC 4648.  When decoding, whitespace characters on input are ignored.',&
'                                                                        ',&
'   With no FILE, or when FILE is "-", data is read from standard input. ',&
'                                                                        ',&
'                                                                        ',&
'   To ignore all bytes not in the formal base64 alphabet, use           ',&
'   --ignore-garbage. This option will attempt to recover from any other ',&
'   non-alphabet bytes in the encoded stream.                            ',&
'                                                                        ',&
'OPTIONS                                                                 ',&
'                                                                        ',&
'    filename             name of file to encode                         ',&
'    --decode,-d          decode instead of encode data                  ',&
'    --ignore-garbage,i   when decoding, ignore non-alphabet characters  ',&
'    --wrap=COLS,-w COLS  wrap encoded lines after COLS characters       ',&
'                         (default 76). Use 0 to disable line wrapping   ',&
'    --version,-v         Print version information on standard output then',&
'                         exit successfully.                               ',&
'    --help,-h            Print usage information on standard output then  ',&
'                         exit successfully.                               ',&
'EXAMPLE                                                                   ',&
'   Sample commands                                                        ',&
'                                                                          ',&
'    base64- input > output.base64                                         ',&
'                                                                          ',&
'SEE ALSO                                                                  ',&
'    base64(1), uuencode(1), uudecode(1)                                   ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        base64-(1f)                                         ',&
'DESCRIPTION:    encode a file to the base 64 encoding standard defined in RFC-4648',&
'VERSION:        1.0, 2024-11-11                                                   ',&
'AUTHOR:         John S. Urban                                                     ',&
'LICENSE:        MIT                                                               ',&
'']
end subroutine setup

end program demo_base64

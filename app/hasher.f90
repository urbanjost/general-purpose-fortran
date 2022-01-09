subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                                                                            ',&
'   hasher(1f) - [M_hashkeys] exercise the string hash methods in the M_hashkey(3fm) module                                      ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   hasher [ input_files [ -hash hashname] ]|[ -string string_value]|--help|--version                                            ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   hasher(1f) does a byte by byte hash of a file or a hash of a string                                                          ',&
'   using the procedures available in the M_hashkey(3fm) module. It is up                                                        ',&
'   to the user to determine if the method is suitable for a specific use.                                                       ',&
'                                                                                                                                ',&
'   When the I/O was not tuned at all and a simple byte-by-byte read                                                             ',&
'   was used the program was sixty times slower; in addition the                                                                 ',&
'   anything_to_bytes(3f) function was slower than expected processing                                                           ',&
'   one character at a time, so if you are going to do anything similar                                                          ',&
'   it is at least worth a look to look at the sample code.                                                                      ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   input_files  files to generate a hash for                                                                                    ',&
'   hash         name of hash algorithm. Currently allowed                                                                       ',&
'                values are:                                                                                                     ',&
'                                                                                                                                ',&
'                  djb2 (default)   calls djb2_hash(3f)                                                                          ',&
'                  sdbm             calls sdbm_hash(3f)                                                                          ',&
'                  crc32            calls cfc32_hash(3f)                                                                         ',&
'                                                                                                                                ',&
'   --help       display this help and exit                                                                                      ',&
'   --version    output version information and exit                                                                             ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        hasher(1f)>',&
'@(#)DESCRIPTION:    string hash example>',&
'@(#)VERSION:        20180928>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to>',&
'@(#)                change and redistribute it.  There is NO WARRANTY;>',&
'@(#)                without even the implied warranty of MERCHANTABILITY or>',&
'@(#)                FITNESS FOR A PARTICULAR PURPOSE.>',&
'@(#)COMPILED:       2022-01-03 20:18:33 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program hash_exe
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT            ! access computing environment
use,intrinsic :: iso_fortran_env, only : iostat_end
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
use M_hashkeys,                   only : djb2_hash, int128
use M_hashkeys,                   only : sdbm_hash
use M_hashkeys,                   only : crc32_hash
use M_kracken,                    only : kracken,lget,sget,sgets
use M_system,                     only : system_isreg
use M_verify,                     only : debug
implicit none
integer                         :: i
integer                         :: ios
integer                         :: icount
integer(kind=int128)            :: hash
integer,parameter               :: IUNIT=15       ! input file unit
character(len=:),allocatable    :: string
character(len=4096),allocatable :: filenames(:)
character(len=4096)             :: msg
character(len=:),allocatable    :: hashname
logical                         :: cont
integer,parameter               :: bufsize=1048576*32
character(len=1)                :: buff(bufsize)
integer                         :: sz
integer                         :: filepoint
abstract interface
   function hashkey (anything,continue)
      import int128
      integer(kind=int128)        :: hashkey
      class(*),intent(in)         :: anything(:)
      logical,intent(in),optional :: continue
   end function hashkey
end interface
procedure (hashkey), pointer :: hash_ptr => null ()
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('hasher',' -hash djb2 -string -version F -help F -verbose F')  ! crack command-line
   call help_usage(lget('hasher_help'))                                        ! check if help requested
   call help_version(lget('hasher_version'))                                   ! check if version requested
   string=trim(sget('hasher_string'))
   filenames=sgets('hasher_oo')
   debug=lget('hasher_verbose')
   hashname=trim(sget('hasher_hash'))
!!  select case(hashname)                                                       ! point to selected function
!!  case('djb2'); hash_ptr => djb2_hash
!!  case('sdbm'); hash_ptr => sdbm_hash
!!  case default
!!     write(ERROR_UNIT,*)'*hasher* error: unknown hash method '
!!     stop 3
!!  end select
!-----------------------------------------------------------------------------------------------------------------------------------
   if(string.ne.'')then
      !!write(*,'(i0,1x,a)')hash_ptr(string),trim(string)
      select case(hashname)                                                       ! point to selected function
      case('skip') ; write(*,'(a,1x,i0.11,1x,i15,1x,a)')hashname, 0                  ,len(string),string
      case('djb2');  write(*,'(a,1x,i0.20,1x,i15,1x,a)')hashname, djb2_hash(string)  ,len(string),string
      case('sdbm');  write(*,'(a,1x,i0.20,1x,i15,1x,a)')hashname, sdbm_hash(string)  ,len(string),string
      case('crc32'); write(*,'(a,1x,i0.11,1x,i15,1x,a)')hashname, crc32_hash(string) ,len(string),string
      case default
         write(ERROR_UNIT,*)'*hasher* error: unknown hash method ',trim(hashname)
         stop 2
      end select
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) then
      write(*,*)'FILES=',(trim(filenames(i)),i=1,size(filenames))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   FILES: do i=1,size(filenames)

   ! open file if it is a regular file
      if(.not.system_isreg(filenames(i)))cycle
      open(unit=IUNIT,file=filenames(i),status='old',access='stream',iostat=ios,iomsg=msg,action='read')
      if(ios.ne.0)then
         write(ERROR_UNIT,'(*(a))')'*hasher* error: file ',trim(filenames(i)),' message=',trim(msg)
         close(unit=IUNIT,iostat=ios)
         cycle FILES
      endif

      cont=.false.                                                 ! for initial call use default seed
      icount=0                                                     ! count number of characters processed
      sz=bufsize
      filepoint=1
      hash=0_int128
      INFINITE: do                                                 ! read and sum file string by string
         ! quick buffering read because reading a single character sixty times slower on CygWIn with gfortran (no system buffering?)
         read(iunit,iostat=ios,pos=filepoint) buff(1:sz)
         if(is_iostat_end(ios))then                                ! this is the last buffer
            if(sz.ne.1)then                                        ! try again with a smaller buffer
               sz=max(1,sz/2)
               cycle INFINITE
            endif
         elseif(ios == 0)then                                      ! no error occurred so successfully read a buffer
             filepoint=filepoint+sz
         endif
         if(ios /= 0)then                                          ! quit reading on error
            exit INFINITE
         endif

         !!hash=hash_ptr(buff(:sz),continue=cont)                    ! build up hash
         select case(hashname)                                     ! point to selected function
         case('skip');
         case('djb2');  hash=djb2_hash(buff(:sz),continue=cont)    ! build up hash
         case('sdbm');  hash=sdbm_hash(buff(:sz),continue=cont)    ! build up hash
         case('crc32'); hash=crc32_hash(buff(:sz),continue=cont)   ! build up hash
         case default
            write(ERROR_UNIT,*)'*hasher* error: unknown hash method ',trim(hashname)
            stop 1
         end select
         icount=icount+sz
         if(cont.eqv..false.)cont=.true.                         ! change flag to continue hash build after first call to accumulate
      enddo INFINITE

      if(icount.eq.0) then
         write(ERROR_UNIT,'(*(a))')'*hasher* error: file ',trim(filenames(i)),' is empty'
      else
         select case(hashname)                                                       ! point to selected function
         case('djb2','sdbm');
            write(*,'(a,1x,i0.20,1x,i15,1x,a)')trim(hashname),hash,icount,trim(filenames(i))
         case('crc32');
            write(*,'(a,1x,i0.11,1x,i15,1x,a)')trim(hashname),hash,icount,trim(filenames(i))
         case('skip');
            hash=0_int128
            write(*,'(a,1x,i0.11,1x,i15,1x,a)')trim(hashname),hash,icount,trim(filenames(i))
         end select
      endif

      close(unit=IUNIT,iostat=ios)

   enddo FILES
end program hash_exe

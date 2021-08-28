program flower
! [dependencies]
! M_kracken      = { git = "https://github.com/urbanjost/M_kracken.git" }
! M_strings      = { git = "https://github.com/urbanjost/M_strings.git" }
! M_io           = { git = "https://github.com/urbanjost/M_io.git" }

use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_io,                         only : get_next_char
use M_kracken,                    only : kracken,lget,sget,sgets
implicit none

! ident_1="@(#)flower(1f): convert basic free-format Fortran file to lowercase"

character(len=4096),allocatable :: filenames(:)      ! array of filenames to read
character(len=:),allocatable    :: filename          ! array of filenames to read
character(len=256)              :: message           ! message field for returned messages
integer,parameter               :: fd=10             ! file descriptor for file currently being read
integer                         :: ios               ! hold I/O error flag
character                       :: c1                ! current character read
character                       :: previous          ! previous significant character
character                       :: last              ! last character read
character,parameter             :: nl=NEW_LINE('A')
integer                         :: ios1              ! hold I/O error flag
integer                         :: icount  = 0       ! number of characters read from file
integer                         :: icount_comm  = 0  ! number of characters read from file that are comments
integer                         :: ipos=0
integer                         :: idiff   = 0       ! number of characters different in files
integer                         :: ilines  = 0       ! number of newline characters encountered in first file
integer                         :: atleast
integer                         :: i
integer                         :: io
logical                         :: incomment
logical                         :: insingle
logical                         :: indouble
logical                         :: tolower
logical                         :: verbose
logical                         :: nocomment
logical                         :: nocode

! define command arguments and parse command line
call kracken('flower',' --version .F. --help .F. --toupper .F. --verbose .F. --nocomment .F. --nocode .F.')
call help_usage(lget('flower_help'))
call help_version(lget('flower_version'))

verbose=lget('flower_verbose')
nocomment=lget('flower_nocomment')
nocode=lget('flower_nocode')
tolower=.not.lget('flower_toupper')

filenames=sgets('flower_oo')                     ! get filenames to scan from command line
if(size(filenames).lt.1)then
   write(stderr,'(a)')'*flower* ERROR: missing filename.'
   stop 4
endif
FILES: do i=1,size(filenames)
   icount  = 0       ! number of characters read from file
   icount_comm  = 0  ! number of characters read from file that are comments
   ipos=0
   idiff   = 0       ! number of characters different in files
   ilines  = 0       ! number of newline characters encountered in first file
   filename=trim(filenames(i))

   close(unit=fd,iostat=ios)
   open(unit=fd,file=filename,access='stream',status='old',iostat=ios,action='read',form='unformatted',iomsg=message)
   if(ios.ne.0)then
      write(stderr,'(a)') '*flower* ERROR: could not open '//filename
      write(stderr,'(a)') '*flower* ERROR: '//trim(message)
      cycle FILES
   endif

   ! reading one character at a time is much more limiting than reading lines or tokens
   ! but is sufficient for the vast majority of cases
   !
   previous=achar(0)
   last=achar(0)
   incomment=.false.
   insingle=.false.
   indouble=.false.
   ONE_CHAR_AT_A_TIME: do                                                ! loop through read of file one character at a time
      call get_next_char(fd,c1,ios1)                                     ! get next character from buffered read from file
      if(ios1.eq.iostat_end)then                                         ! reached end of file so stop
         if(verbose)then
            if(nocode.and.nocomment)then
               io=stdout
            else
               io=stderr
            endif
            atleast=max(len(filename),32)
            write(io,'(*(g0:,1x))',advance='no')[character(len=atleast) :: filename]
            write(io,'(*(g0:,1x,i8))',advance='no')' lines', ilines
            write(io,'(*(g0:,1x,i8))',advance='no')' total ', icount
            write(io,'(*(g0:,1x,i8))',advance='no')' comments', icount_comm
            write(io,'(*(g0:,1x,f5.2))',advance='no')' % comments', real(icount_comm*100)/real(icount)
            write(io,'(*(g0:,1x,i8))',advance='no')' case changes', idiff
            write(io,*)
         endif
         cycle FILES
      elseif(ios1.ne.0 )then                                             ! error or end of file
         write(stderr,*)'*flower* ERROR: EOF or error on '//filename//' before end of '//filename
         cycle FILES
      endif
      icount=icount+1                                                    ! increment count of characters read
      select case(c1)
      case('!')
         if(any([insingle,indouble]))then
            continue
          else
            incomment=.true.
         endif
      case('&')
      case('"')
         if(any([incomment,insingle]))then
            continue
         elseif(indouble)then
            indouble=.false.
         else
            indouble=.true.
         endif
      case("'")
         if(any([incomment,indouble]))then
            continue
         elseif(insingle)then
            insingle=.false.
         else
            insingle=.true.
         endif
      case(nl)
         ilines=ilines+1                             ! increment count of newline characters in file
         if(previous.ne.'&')then
            incomment=.false.
            insingle=.false.
            indouble=.false.
         elseif(incomment)then
            incomment=.false.
         endif
      case('a':'z')
         if(.not.tolower)then
            if(.not.any([incomment,insingle,indouble]))then
               c1=char(iachar(c1)-32)
               idiff=idiff+1
            endif
         endif
      case('A':'Z')
         if(tolower)then
            if(.not.any([incomment,insingle,indouble]))then
               c1=char(iachar(c1)+32)
               idiff=idiff+1
            endif
         endif
      case default
      end select

      if(incomment)then
         icount_comm=icount_comm+1
      endif

      if (c1.eq.nl)then  ! not sure what is best for newlines
         if(nocomment.and.nocode)then
            continue
         elseif(nocomment.and.ipos.eq.0)then
            if(last.eq.nl)write(*,'(*(g0))',advance='no')c1
         elseif(nocode.and.ipos.eq.0)then
            if(last.eq.nl)write(*,'(*(g0))',advance='no')c1
         else
            write(*,'(*(g0))',advance='no')c1
            ipos=0
         endif
      elseif(nocomment.and.incomment)then
         continue
      elseif(nocode.and..not.incomment)then
         continue
      else
         ipos=ipos+1
         write(*,'(a)',advance='no')c1
      endif

      if(c1.ne.' ')then
         if(.not.any([incomment]).and.previous.ne.' ')then
            previous=c1
         endif
      endif
      if(c1.ne.' ') last=c1

   enddo ONE_CHAR_AT_A_TIME
enddo FILES

contains
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
'flower(1f) - [FUNIX] change case of free-format Fortran file                                                                    ',&
'     (LICENSE:PD)                                                                                                               ',&
'SYNOPSIS                                                                                                                        ',&
'     flower FILENAME [--verbose] [--nocomment|--nocode]                                                                         ',&
'            [--toupper]|[--help|--version]                                                                                      ',&
'DESCRIPTION                                                                                                                     ',&
'     Convert the free-format Fortran source file to lowercase or uppercase                                                      ',&
'     leaving comments and quoted text as-is. This is a basic program                                                            ',&
'     that writes its results to stdout and does not recognize Hollerith                                                         ',&
'     strings and preprocessor directives as special cases.                                                                      ',&
'                                                                                                                                ',&
'     Tabs should be expanded before processing the file                                                                         ',&
'                                                                                                                                ',&
'     This is a very simplistic approach so the output should be carefully                                                       ',&
'     checked.                                                                                                                   ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'     FILENAME     Fortran source file which is to be converted to lowercase                                                     ',&
'     --nocomment  remove comment characters                                                                                     ',&
'     --nocode     remove code characters                                                                                        ',&
'     --toupper    convert code characters to uppercase instead of lowercase                                                     ',&
'     --verbose    turn on verbose mode                                                                                          ',&
'     --help       display help text and exit                                                                                    ',&
'     --version    display version text and exit                                                                                 ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'   Typical usage                                                                                                                ',&
'                                                                                                                                ',&
'     # convert old code to lowercase                                                                                            ',&
'     flower sample.f90 > sample_new.f90                                                                                         ',&
'                                                                                                                                ',&
'     # extract all code comments and do a spell check                                                                           ',&
'     flower *.f90 -nocode|spell                                                                                                 ',&
'                                                                                                                                ',&
'     # show stats for files measuring percent of comments                                                                       ',&
'     flower *.f90 --nocode --nocomment --verbose                                                                                ',&
'                                                                                                                                ',&
'EXIT STATUS                                                                                                                     ',&
'     The following exit values are returned:                                                                                    ',&
'                                                                                                                                ',&
'      0     no differences were found                                                                                           ',&
'      1     differences were found                                                                                              ',&
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
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        flower(1)>',&
'@(#)DESCRIPTION:    convert free-format Fortran source to lowercase>',&
'@(#)VERSION:        1.0-20171126>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2021-08-28 05:32:11 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program flower

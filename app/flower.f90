! always reducing multiple blank lines to one; maybe only do that if other lines were comments not if originally blank
program flower
! [dependencies]
! M_CLI2         = { git = "https://github.com/urbanjost/M_CLI2.git" }
! M_strings      = { git = "https://github.com/urbanjost/M_strings.git" }
! M_io           = { git = "https://github.com/urbanjost/M_io.git" }

use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_io,                         only : get_next_char
use M_CLI2,                       only : set_args,lget,sget,sgets, filenames=>unnamed
implicit none

! ident_1="@(#) flower(1f) convert basic free-format Fortran file to lowercase or uppercase"

character(len=:),allocatable    :: filename
character(len=:),allocatable    :: help_text(:)
character(len=:),allocatable    :: version_text(:)
character(len=:),allocatable    :: outline
character(len=:),allocatable    :: outlinel
character(len=256)              :: message           ! message field for returned messages
integer,parameter               :: fd=10             ! file descriptor for file currently being read
integer                         :: ios               ! hold I/O error flag
character                       :: c1                ! current character read
character                       :: previous          ! previous significant character
character,parameter             :: nl=new_line('A')
integer                         :: ios1              ! hold I/O error flag
integer                         :: icount  = 0       ! number of characters read from file
integer                         :: icount_comm  = 0  ! number of characters read from file that are comments
integer                         :: idiff   = 0       ! number of characters different in files
integer                         :: ilines  = 0       ! number of newline characters encountered in first file
integer                         :: iposition  = 0    ! number of characters read from current line
integer                         :: atleast
integer                         :: i
integer                         :: io
logical                         :: incomment
logical                         :: insingle
logical                         :: indouble
logical                         :: tolower
logical                         :: ifblank
logical                         :: verbose
logical                         :: nocomment
logical                         :: nocode
logical                         :: nocont
character(len=3)                :: advance
integer                         :: ilen

call setup()
call set_args('--toupper F --nocomment F --nocode F --nocont F --stat F',help_text,version_text)
verbose=lget('verbose')
nocomment=lget('nocomment')
nocode=lget('nocode')
tolower=.not.lget('toupper')
nocont=lget('nocont')
if(lget('stat'))then
  verbose=.true.
  nocomment=.true.
  nocode=.true.
endif
if(nocomment.and.nocode)verbose=.true.
ifblank=.false.

if(size(filenames).lt.1)then
   write(stderr,'(a)')'*flower* ERROR: missing filename.'
   stop 4
endif
FILES: do i=1,size(filenames)
   icount  = 0       ! number of characters read from file
   icount_comm  = 0  ! number of characters read from file that are comments
   idiff   = 0       ! number of characters different in files
   ilines  = 0       ! number of newline characters encountered in first file
   filename=trim(filenames(i))
   outline=''

   close(unit=fd,iostat=ios)
   open(unit=fd,file=filename,access='stream',status='old',iostat=ios,action='read',form='unformatted',iomsg=message)
   if(ios.ne.0)then
      write(stderr,'(a)') '*flower* ERROR: could not open '//filename
      write(stderr,'(a)') '*flower* ERROR: '//trim(message)
      cycle FILES
   endif

   if(verbose)then
      if(nocomment.and.nocode)then
         continue
      elseif(nocomment)then
         write(stdout,'(/,a,a)') '!filename:',filename
      elseif(nocode)then
         write(stdout,'(/,a,a)') 'filename: ',filename
      endif
   endif

   ! reading one character at a time is much more limiting than reading lines or tokens
   ! but is sufficient for the vast majority of cases
   !
   iposition=0
   previous=achar(0)
   incomment=.false.
   insingle=.false.
   indouble=.false.
   advance='yes'
   ONE_CHAR_AT_A_TIME: do                                                ! loop through read of file one character at a time
      call get_next_char(fd,c1,ios1)                                     ! get next character from buffered read from file
      if(ios1.eq.iostat_end)then                                         ! reached end of file so stop
         if(verbose)then
            if(nocode.and.nocomment)then
               io=stdout
            else
               io=stderr
            endif
            if(size(filenames).gt.1)then
               atleast=max(len(filenames),4)
               write(io,'(g0:,1x)',advance='no')[character(len=atleast) :: filename]
               write(io,'(g0:,1x,i8)',advance='no')' lines', ilines
               write(io,'(g0:,1x,i8,"b")',advance='no')' total ', icount
               write(io,'(g0:,1x,i8,"b")',advance='no')' comments', icount_comm
               write(io,'(g0:,1x,f5.2)',advance='no')' % comments', real(icount_comm*100)/real(icount)
               write(io,'(g0:,1x,i8,"b")',advance='no')' case changes', idiff
               write(io,*)
            else
               write(io,'(g0:,1x)',advance='no')filename
               write(io,'(g0:,1x,i0)',advance='no')' lines', ilines
               write(io,'(g0:,1x,i0,"b")',advance='no')' total ', icount
               write(io,'(g0:,1x,i0,"b")',advance='no')' comments', icount_comm
               write(io,'(g0:,1x,f5.2)',advance='no')' % comments', real(icount_comm*100)/real(icount)
               write(io,'(g0:,1x,i0,"b")',advance='no')' case changes', idiff
               write(io,*)
            endif
         endif
         cycle FILES
      elseif(ios1.ne.0 )then                                             ! error or end of file
         write(stderr,*)'*flower* ERROR: EOF or error on '//filename//' before end of '//filename
         cycle FILES
      endif
      iposition=iposition+1
      icount=icount+1                                                    ! increment count of characters read
      select case(c1)
      case('#','$')
         if(iposition.eq.1)then  ! will still not save macros, but as a kluge treat these lines as comments (?)
            incomment=.true.
         endif
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
         iposition=0
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

      if (c1.eq.nl)then                     ! remove adjacent blank lines
         if(nocomment.and.nocode)then
    continue
         else
            if(nocode) then
       outline=outline//' '
               outline=trim(outline(max(1,verify(outline,'!')):)) ! trim leading exclamations
            endif
            if(incomment)then
               continue
            elseif(previous.eq.'&'.and.nocont)then
               advance='no'
               outline=trim(outline)
               ilen=len(outline)
               if(outline(ilen:ilen).eq.'&')then
                  outline=outline(:ilen-1)
               endif
            else
               advance='yes'
            endif
            if(outline.eq.''.or.adjustl(outline).eq.achar(10))then  ! if a blank line output if previous line not a blank line
               if(ifblank)then
                  continue
               else
                  write(*,'(a)',advance=advance)
               endif
               ifblank=.true.
            else                                                    ! print a non-blank line
               outlinel=adjustl(outline)
               if(index(outlinel,'&').eq.1.and.nocont)then
                 outline='#'//outlinel(2:)
               endif
               write(*,'(a)',advance=advance)outline
               ifblank=.false.
            endif
         endif
         outline=''
      elseif(nocomment.and.incomment)then
         continue
      elseif(nocode.and..not.incomment)then
         continue
      elseif(incomment.or.insingle.or.indouble)then
         outline=outline//c1
      elseif(c1.eq.'&'.and.nocont)then
         continue
      else
         outline=outline//c1
      endif

      if(c1.ne.' ')then
         if(.not.any([incomment]).and.previous.ne.' ')then
            previous=c1
         endif
      endif

   enddo ONE_CHAR_AT_A_TIME

   if(idiff.ne.0)then
      stop 1
   else
      stop 0
   endif

enddo FILES

contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'flower(1f) - [DEVELOPER] change case of free-format Fortran file',&
'             or remove code or remove comments',&
'             (LICENSE:PD)',&
'SYNOPSIS',&
'   flower [ --stat|[ [--nocomment|--nocode] [--toupper]|[--verbose] ]',&
'          FILENAMES(s) ]|[--help|--version|--usage]',&
'DESCRIPTION',&
'',&
'',&
'',&
'1. flower(1) will convert free-format Fortran code to lowercase.',&
'   It can also convert the code to uppercase. In each case comments and',&
'   quoted text are left as-is.',&
'',&
'   This is a basic program that writes its results to stdout and does',&
'   not recognize Hollerith strings and preprocessor directives as',&
'   special cases.',&
'',&
'   Tabs should be expanded before processing the file.',&
'',&
'   flower(1) depends on the input being plain standard free-format',&
'   Fortran so the output should be carefully checked.',&
'',&
'2. It may also be used to generate simple statistics about what percentage',&
'   of the code is comments.',&
'',&
'3. flower(1) can also be used to strip comments from the code.',&
'',&
'4. Lastly, the code can be removed so the comments can be used for documentation',&
'   or run through utilities like spell checkers.',&
'',&
'OPTIONS',&
'     FILENAME     Fortran source file which is to be converted to lowercase',&
'     --nocomment  remove comment characters',&
'     --nocode     remove code characters',&
'     --toupper    convert code characters to uppercase instead of lowercase',&
'     --verbose    turn on verbose mode including file statistics. Note',&
'                  that if --nocomment and --nocode are selected --verbose',&
'                  is implied.',&
'     --stat       is the same as --nocomment --nocode --verbose, meaning',&
'                  no other output than file statistics will be produced.',&
'                  If present, --nocomment, --nocode, and --verbose are',&
'                  ignored.',&
'',&
'     --help       display help text and exit',&
'     --version    display version text and exit',&
'',&
'EXAMPLES',&
'   Typical usage',&
'',&
'     # convert all code to lowercase',&
'     flower sample.f90 > sample_new.f90',&
'',&
'     # show stats for files measuring percent of comments',&
'     flower --stat *.f90 *.F90',&
'',&
'     # check spelling on comments',&
'     flower --nocode *.f90 *.F90|spell',&
'',&
'     # grep code ignoring comments',&
'     flower --nocomment *.f90 *.F90|grep -iw contains',&
'',&
'EXIT STATUS',&
'   The following exit values are returned:',&
'',&
'      0     no differences were found',&
'      1     differences were found',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        flower(1)>',&
'@(#)DESCRIPTION:    convert free-format Fortran source to lowercase>',&
'@(#)VERSION:        1.0-20171126>',&
'@(#)AUTHOR:         John S. Urban>',&
'']
end subroutine setup

end program flower

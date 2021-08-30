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
'     flower(1f) - [FUNIX] convert free-format Fortran file to lowercase                                                         ',&
'     (LICENSE:PD)                                                                                                               ',&
'SYNOPSIS                                                                                                                        ',&
'     flower FILENAME                                                                                                            ',&
'DESCRIPTION                                                                                                                     ',&
'     Convert the free-format Fortran source file to lowercase leaving                                                           ',&
'     comments and quoted text as-is. This is a basic program that writes                                                        ',&
'     its results to stdout and does not recognize Hollerith strings and                                                         ',&
'     preprocessor directives as special cases.                                                                                  ',&
'                                                                                                                                ',&
'     tabs should be expanded before processing the file                                                                         ',&
'                                                                                                                                ',&
'     This is a very simplistic approach so the output should be carefully                                                       ',&
'     checked.                                                                                                                   ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'     --help     display help text and exit                                                                                      ',&
'     --version  display version text and exit                                                                                   ',&
'EXAMPLES                                                                                                                        ',&
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
'@(#)COMPILED:       2021-08-26 20:05:38 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program flower
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_io,                         only : get_next_char
use M_kracken,                    only : kracken,lget,sget,sgets
implicit none

! ident_1="@(#)flower(1f): convert basic free-format Fortran file to lowercase"

character(len=4096),allocatable :: filenames(:)      ! array of filenames to read
character(len=4096),allocatable :: filename          ! array of filenames to read
character(len=256)              :: message           ! message field for returned messages
integer                         :: fd                ! file descriptor for file currently being read
integer                         :: ios               ! hold I/O error flag
integer                         :: i                 ! loop counter
character                       :: c1,previous       ! current character read
integer                         :: ios1              ! hold I/O error flag
integer                         :: icount  = 0       ! number of characters read from file
integer                         :: idiff   = 0       ! number of characters different in files
integer                         :: ilines  = 0       ! number of newline characters encountered in first file
logical                         :: incomment
logical                         :: insingle
logical                         :: indouble
logical                         :: tolower
!-----------------------------------------------------------------------------------------------------------------------------------
! define command arguments and parse command line
call kracken('flower',' -version .F. -help .F. --toupper .F.')
call help_usage(lget('flower_help'))
call help_version(lget('flower_version'))
filenames=sgets('flower_oo')                     ! get filenames to scan from command line
if(size(filenames).lt.1)then
   write(stderr,'(a)')'*flower* ERROR: missing filename.'
   stop 4
endif
tolower=.not.lget('flower_toupper')
filename=filenames(1)
!-----------------------------------------------------------------------------------------------------------------------------------
   open(unit=fd,file=trim(filename),access='stream',status='old',iostat=ios,action='read',form='unformatted',iomsg=message)
   if(ios.ne.0)then
      write(stderr,'(a)') '*flower* ERROR: could not open '//trim(filename)
      write(stderr,'(a)') '*flower* ERROR: '//trim(message)
      stop 5
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
! reading one character at a time is much more limiting than reading lines or tokens
! but is sufficient for the vast majority of cases
!
previous=achar(0)
incomment=.false.
insingle=.false.
indouble=.false.
ONE_CHAR_AT_A_TIME: do                                                ! loop through read of file one character at a time
   call get_next_char(fd,c1,ios1)                                     ! get next character from buffered read from file

   if(ios1.eq.iostat_end)then                                         ! reached end of file so stop
      if(idiff.eq.0)then                                              ! message for no changes
            write(stderr,'(*(a:,1x))',advance='no')trim(filename),' not changed'
      else                                                            ! messages when changes occurred
            write(stderr,'(*(a:,1x))',advance='no')trim(filename),' are different by '
            write(stderr,'(i0," bytes")')idiff
      endif
      stop
   elseif(ios1.ne.0 )then                                             ! error or end of file
      write(stderr,*)'*flower* ERROR: EOF or error on '//trim(filename)//' before end of '//trim(filename)
      stop 1
   endif
   icount=icount+1                                                    ! increment count of characters read
   if(c1.eq.NEW_LINE('A'))ilines=ilines+1                             ! increment count of newline characters in file
   select case(c1)
   case('!')
      if(.not.all([insingle,indouble]))then
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
   case(NEW_LINE('A'))
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
   write(*,'(a)',advance='no')c1
   if(c1.ne.' ')then
      if(.not.any([incomment]))then
         previous=c1
      endif
   endif
enddo ONE_CHAR_AT_A_TIME
!-----------------------------------------------------------------------------------------------------------------------------------
end program flower
!-----------------------------------------------------------------------------------------------------------------------------------

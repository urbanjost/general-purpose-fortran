!===================================================================================================================================
program expand
use M_kracken, only: kracken, lget, iget, sget
use M_strings, only: notabs, split
implicit none

! ident_1="@(#)expand-(1f):filter removes tabs and trailing white space from files up to 1024 chars wide"

character(len=1024)              :: in,out
integer                          :: ios          ! error flag from read
integer                          :: iout
integer                          :: i,ii
integer                          :: ierror=0
character(len=:),allocatable     :: array(:)     ! split name of filenames
integer                          :: iblanks
integer                          :: maxblanks
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('expand','-help .F. -blanks -1 -version .F. ',ierror) ! define and crack command arguments and values
   call help_usage(lget('expand_help'))                               ! check if -help was specified
   call help_version(lget('expand_version'))                          ! check if -version was specified
!-----------------------------------------------------------------------------------------------------------------------------------
   if(sget('expand_oo').ne.'')then                         ! see if filenames were listed on the command line
      call split(sget('expand_oo'),array)                  ! split the filenames into an array
   else                                                    ! default is to read from stdin, which the filename "-" designates
      array=['-']
   endif
   maxblanks=iget('expand_blanks')
!-----------------------------------------------------------------------------------------------------------------------------------
   ALLFILES: do i=1,size(array)                            ! loop through all the filenames
      if(array(i).eq.'-')then                              ! special filename designates stdin
         ii=5
      else                                                 ! open a regular file
         ii=20
         open(unit=ii,file=trim(array(i)),iostat=ios,status='old',form='formatted')
         if(ios.ne.0)then
            !call stderr('*expand-* failed to open:'//trim(array(i)))
            cycle ALLFILES
         endif
      endif
      iblanks=0
      ALLLINES: do                                         ! loop thru the file and call notabs(3f) on each line
         read(ii,"(a)",iostat=ios)in
         if(ios /= 0)then
            exit ALLLINES
         endif
         call notabs(in,out,iout)
         if(maxblanks.ge.0)then
            if(out(:iout).eq.'')then
               iblanks=iblanks+1
               if(iblanks.gt.maxblanks)then
                  cycle ALLLINES
               endif
            else
               iblanks=0
            endif
         endif
         write(*,"(a)")out(:iout)
      enddo ALLLINES
      close(unit=20,iostat=ios)
   enddo ALLFILES
!===================================================================================================================================
contains
!===================================================================================================================================
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
'       expand-(1f) - [FUNIX] convert tabs to spaces                                                                             ',&
'       (LICENSE:PD)                                                                                                             ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'       expand- FILENAME(S) [ -blanks N]| --help| --version                                                                      ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'       Convert tabs in each FILE to spaces, writing to standard output.                                                         ',&
'       If no filename is specified standard input is read. Tab stops                                                            ',&
'       are assumed to be every eight (8) columns. Trailing spaces,                                                              ',&
'       carriage returns, and newlines are removed.                                                                              ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'       FILENAMES   files to expand tab characters in.                                                                           ',&
'       -blanks     maximum number of adjacent blank lines to retain.                                                            ',&
'                   Default is -1, which is equivalent to unlimited.                                                             ',&
'       --help      display this help and exit                                                                                   ',&
'       --version   output version information and exit                                                                          ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'       Sample commands:                                                                                                         ',&
'                                                                                                                                ',&
'        expand- < input.txt > output.txt                                                                                        ',&
'        expand- input.txt   > output.txt                                                                                        ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!        expand-(1f) - [FUNIX] convert tabs to spaces
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        expand- FILENAME(S) [ -blanks N]| --help| --version
!!
!!##DESCRIPTION
!!        Convert tabs in each FILE to spaces, writing to standard output.
!!        If no filename is specified standard input is read. Tab stops
!!        are assumed to be every eight (8) columns. Trailing spaces,
!!        carriage returns, and newlines are removed.
!!
!!##OPTIONS
!!        FILENAMES   files to expand tab characters in.
!!        -blanks     maximum number of adjacent blank lines to retain.
!!                    Default is -1, which is equivalent to unlimited.
!!        --help      display this help and exit
!!        --version   output version information and exit
!!
!!##EXAMPLES
!!
!!        Sample commands:
!!
!!         expand- < input.txt > output.txt
!!         expand- input.txt   > output.txt
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
'@(#)PROGRAM:        expand-(1f)>',&
'@(#)DESCRIPTION:    convert tabs to spaces>',&
'@(#)VERSION:        1.0, 2015-12-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2021-12-18 15:27:52 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program expand

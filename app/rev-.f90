program rev
use M_io,      only : notopen, read_line
use M_verify,   only : stderr
use M_kracken, only : kracken, sgets, lget
use M_strings, only : reverse
implicit none
! ident_1="@(#)reverse lines in a file"
integer                            :: ios                                        ! I/O error flag
integer                            :: iputunit                                   ! unit number for output file
character(len=:),allocatable       :: files(:)                                   ! array to hold files from command line
character(len=:),allocatable       :: line
character(len=1024)                :: message
integer                            :: i
   call kracken('rev',' -help .false. -version .false.')                         ! crack command line
   call help_usage(lget('rev_help'))                                             ! check if -help present
   call help_version(lget('rev_version'))                                        ! check if -version present
   files=sgets('rev_oo')                                                         ! get filename(s) from command line
   if(size(files)==0)then
      files=['-']
   endif
   do i=1,size(files)                                                            ! for each file read and reverse lines
      if(files(i)=='-')then                                                      ! for filename - use stdin
         iputunit=5
      else
         open(newunit=iputunit,file=files(i),iostat=ios,iomsg=message,status='old',action='read')
         if(ios/=0)then
            call stderr('*rev-* error opening ',files(i),'==>',message)
            cycle
         endif
      endif
      INFINITE: do while (read_line(line,lun=iputunit)==0)
         write(*,'(a)')reverse(line)
      enddo INFINITE
   enddo
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
'       rev-(1f) - [FUNIX] reverse lines in a file                                                                               ',&
'       (LICENSE:PD)                                                                                                             ',&
'SYNOPSIS                                                                                                                        ',&
'       rev- INPUT_FILE(S) [ --help][ --version]                                                                                 ',&
'DESCRIPTION                                                                                                                     ',&
'       reverse lines in a file                                                                                                  ',&
'OPTIONS                                                                                                                         ',&
'       INPUT_FILE(s)  input file(s)                                                                                             ',&
'       --help         display help text and exit                                                                                ',&
'       --version      display version information and exit                                                                      ',&
'SEE ALSO                                                                                                                        ',&
'       tac(1), rev(1)                                                                                                           ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!        rev-(1f) - [FUNIX] reverse lines in a file
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        rev- INPUT_FILE(S) [ --help][ --version]
!!##DESCRIPTION
!!        reverse lines in a file
!!##OPTIONS
!!        INPUT_FILE(s)  input file(s)
!!        --help         display help text and exit
!!        --version      display version information and exit
!!##SEE ALSO
!!        tac(1), rev(1)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
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
'@(#)PROGRAM:        rev-(1f)>',&
'@(#)DESCRIPTION:    reverse lnes in a file>',&
'@(#)VERSION:        1.0, 2019-08-31>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-01-09 10:18:44 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program rev

program cprint
use M_kracken, only : kracken, igets,  sget, lget, sgets
use M_strings, only : split, listout
use M_io, only : read_line
implicit none

! ident_1="@(#)cprint(1f): filter to print specified columns"

character(len=:),allocatable       :: line
character(len=:),allocatable       :: delimiters               ! characters used to delimit columns
character(len=:),allocatable       :: separator                ! characters used to delimit columns on output
character(len=:),allocatable       :: array(:)
integer,allocatable                :: icols(:)
integer                            :: icols_expanded(1000)     ! output array
integer                            :: isize
integer                            :: i
integer                            :: inums                    ! size of icols on input, number of icols_expanded numbers on output
integer                            :: ierr
logical                            :: verbose
logical                            :: null
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('cprint',      &              ! define command options and default values and then process command-line arguments
   & ' -delimiters  -help .F. -version .F. -verbose .F. --separator "#N#" --null .F. ')
   call help_usage(lget('cprint_help'))       ! if -help option is present, display help text and exit
   call help_version(lget('cprint_version'))  ! if -version option is present, display version text and exit
   delimiters=sget('cprint_delimiters')       ! get -delimiters values
   separator=sget('cprint_separator')         ! get -delimiters values
   if(separator=='#N#') separator=' '
   if(lget('cprint_null')) separator=''
   icols=igets('cprint_oo',ierr)
   inums=size(icols)
   call listout(icols,icols_expanded,inums,ierr)
   verbose=lget('cprint_verbose')
   if(verbose)then
      write(*,'("COLUMNS=",*("[",i0,"]":","))',advance='no')icols
      write(*,'(" DELIMITERS=",*("[",g0,"]":","))',advance='no')delimiters
      write(*,'(" SEPARATOR=",*("[",g0,"]":","))',advance='yes')separator
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do while (read_line(line)==0)
      if(line.ne.'')then
         call split(line,array,delimiters)             ! split line into columns
         isize=min(size(array),1000)
         do i=1,inums
            if( icols_expanded(i).gt.0 .and. icols_expanded(i).le.isize )then
               write(*,'(a,a)',advance='no')trim(array(icols_expanded(i))),separator
            elseif(icols_expanded(i).eq.0)then
               write(*,'(a,a)',advance='no')' ',separator
            endif
         enddo
      endif
      write(*,*)
   enddo INFINITE
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
'NAME                                                                              ',&
'   cprint(1f) - [FILE FILTER] filter prints specified columns                     ',&
'   (LICENSE:PD)                                                                   ',&
'SYNOPSIS                                                                          ',&
'   cprint [ columns ][ --delimiters STR] [--separator STR|--null]                 ',&
'   cprint --help|--version                                                        ',&
'                                                                                  ',&
'DESCRIPTION                                                                       ',&
'   cprint is a filter that prints the specified columns.                          ',&
'                                                                                  ',&
'OPTIONS                                                                           ',&
'   columns       numbers indicating which columns to print.                       ',&
'                 A negative value denotes the end of a range.                     ',&
'                 A value must be from 1 to 1000. Zero (0) just prints a string.   ',&
'   --delimiters  input column delimiter character(s) (default: whitespace)        ',&
'                 Delimiters are changed to spaces on outout.                      ',&
'   --separator   output separator string (default: space)                         ',&
'   --null        flag indicating no separator string is to be used.               ',&
'   --help        display command help and exit                                    ',&
'   --version     display command metadata and exit                                ',&
'EXAMPLES                                                                          ',&
'  Sample usage:                                                                   ',&
'                                                                                  ',&
'      $echo a b c d|cprint 1000 -1 # reverse column order of a table              ',&
'      d c b a                                                                     ',&
'                                                                                  ',&
'      $: switch first and second column and skip third column                     ',&
'      $: and print up to column 1000                                              ',&
'      $ls -l |cprint 2 1 4 -1000                                                  ',&
'                                                                                  ',&
'      $: column numbers may be reused                                             ',&
'      $echo d e h l o r w|cprint 3 2 4 4 5 7 5 6 4 1                              ',&
'      h e l l o w o r l d                                                         ',&
'                                                                                  ',&
'AUTHOR                                                                            ',&
'   John S. Urban                                                                  ',&
'LICENSE                                                                           ',&
'   Public Domain                                                                  ',&
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
'@(#)PROGRAM:        cprint(1f)>',&
'@(#)DESCRIPTION:    filter that prints specified columns>',&
'@(#)VERSION:        1.0, 20170224>',&
'@(#)VERSION:        2.0, 20200526>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2021-12-23 18:10:08 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program
!===================================================================================================================================

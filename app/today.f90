program today
implicit none
   call main()
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
'       today(1f) - [TIME] output current time for uses such as file suffixes.                                                   ',&
'       (LICENSE:PD)                                                                                                             ',&
'SYNOPSIS                                                                                                                        ',&
'       today format|--help|--version|--options                                                                                  ',&
'DESCRIPTION                                                                                                                     ',&
'       Outputs the current date using the specified format. Typically used                                                      ',&
'       to generate a string to be used in building filenames containing                                                         ',&
'       date information.                                                                                                        ',&
'OPTIONS                                                                                                                         ',&
'       format     any allowable format for the fmtdate(3) routine. Enter                                                        ',&
'                  "-" to get a list on stdout. defaults to "YMD".                                                               ',&
'       --help     display this help and exit                                                                                    ',&
'       --version  output version information and exit                                                                           ',&
'       --options  display allowed options for building a format                                                                 ',&
'EXAMPLE                                                                                                                         ',&
'       Sample commands:                                                                                                         ',&
'                                                                                                                                ',&
'        cp myfile myfile.`today`                                                                                                ',&
'        find . -ls > MANIFEST.`today epoch`                                                                                     ',&
'        mkdir `today YMDhms`                                                                                                    ',&
'        today yearmonthdayhourminutesecond                                                                                      ',&
'        today --options                       # show formatting options                                                         ',&
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
!!        today(1f) - [TIME] output current time for uses such as file suffixes.
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        today format|--help|--version|--options
!!##DESCRIPTION
!!        Outputs the current date using the specified format. Typically used
!!        to generate a string to be used in building filenames containing
!!        date information.
!!##OPTIONS
!!        format     any allowable format for the fmtdate(3) routine. Enter
!!                   "-" to get a list on stdout. defaults to "YMD".
!!        --help     display this help and exit
!!        --version  output version information and exit
!!        --options  display allowed options for building a format
!!##EXAMPLE
!!
!!        Sample commands:
!!
!!         cp myfile myfile.`today`
!!         find . -ls > MANIFEST.`today epoch`
!!         mkdir `today YMDhms`
!!         today yearmonthdayhourminutesecond
!!         today --options                       # show formatting options
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
'@(#)PROGRAM:        today(1f)>',&
'@(#)DESCRIPTION:    output current time for uses such as file suffixes.>',&
'@(#)VERSION:        1.0, 2009>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2021-12-18 15:27:08 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
subroutine main()
use M_time,    only : now, fmtdate_usage
use M_kracken95, only : kracken, lget, sget                    ! add command-line parser module

! ident_1="@(#)today(1f): output current time for uses such as file suffixes."

character(len=:),allocatable :: options
   call kracken('today','-help .F. -version .F. -options .F.') ! define command arguments,default values and crack command line
   call help_usage(lget('today_help'))                         ! if -help option is present, display help text and exit
   call help_version(lget('today_version'))                    ! if -version option is present, display version text and exit
   if(lget('today_options'))then                               ! special option to list date format documentation
      call fmtdate_usage()                                     ! see all formatting options
   else
      options= sget('today_oo')                                ! get -oo STRING
      if(options.eq.'')then                                    ! if options are blank set a default
         write(*,'(a)')now('YMD')                              ! display current date using format from command line
      else
         write(*,'(a)')now(options)                            ! display current date using format from command line
      endif
   endif
end subroutine main
end program today

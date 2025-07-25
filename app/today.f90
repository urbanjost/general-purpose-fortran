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
'NAME                                                                            ',&
'       today(1f) - [TIME] output current time for uses such as file suffixes.   ',&
'       (LICENSE:PD)                                                             ',&
'SYNOPSIS                                                                        ',&
'       today format|--help|--version|--test                                     ',&
'DESCRIPTION                                                                     ',&
'       Outputs the current date using the specified format. Typically used      ',&
'       to generate a string to be used in building filenames containing         ',&
'       date information.                                                        ',&
'OPTIONS                                                                         ',&
'       format     any allowable format for the fmtdate(3) routine.              ',&
'                  defaults to "Y-M-D".                                          ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'       --test     display allowed options for building a format                 ',&
'EXAMPLES                                                                        ',&
'       Sample commands:                                                         ',&
'                                                                                ',&
'        $today                                                                  ',&
'        2024-05-27                                                              ',&
'                                                                                ',&
'        $mv -v myfile myfile.`today`                                            ',&
'        renamed ''myfile'' -> ''myfile.2024-05-27''                             ',&
'                                                                                ',&
'        $find . -ls > MANIFEST.`today epoch`; ls MANIFEST.*                     ',&
'        MANIFEST.1716840303                                                     ',&
'                                                                                ',&
'        $mkdir `today YMDhms`                                                   ',&
'        20240527160333                                                          ',&
'                                                                                ',&
'        $today yearmonthdayhourminutesecond                                     ',&
'        20240527160442                                                          ',&
'                                                                                ',&
'        $today --test                          # show formatting options        ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
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
!!        today format|--help|--version|--test
!!##DESCRIPTION
!!        Outputs the current date using the specified format. Typically used
!!        to generate a string to be used in building filenames containing
!!        date information.
!!##OPTIONS
!!        format     any allowable format for the fmtdate(3) routine.
!!                   defaults to "Y-M-D".
!!        --help     display this help and exit
!!        --version  output version information and exit
!!        --test     display allowed options for building a format
!!##EXAMPLES
!!
!!        Sample commands:
!!
!!         $today
!!         2024-05-27
!!
!!         $mv -v myfile myfile.`today`
!!         renamed 'myfile' -> 'myfile.2024-05-27'
!!
!!         $find . -ls > MANIFEST.`today epoch`; ls MANIFEST.*
!!         MANIFEST.1716840303
!!
!!         $mkdir `today YMDhms`
!!         20240527160333
!!
!!         $today yearmonthdayhourminutesecond
!!         20240527160442
!!
!!         $today --test                          # show formatting options
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
'@(#)VERSION:        1.0, 2009, 1.0.1 2024>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2025-06-29 08:19:11 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
subroutine main()
use M_time,    only : now, fmtdate_usage, locale
use M_kracken95, only : kracken, lget, sget                    ! add command-line parser module

! ident_1="@(#) today(1f) output current time for uses such as file suffixes."

character(len=:),allocatable :: options
   call locale('LANGUAGE')
   call kracken('today','-help .F. -version .F. -test .F.')    ! define command arguments,default values and crack command line
   call help_usage(lget('today_help'))                         ! if -help option is present, display help text and exit
   call help_version(lget('today_version'))                    ! if -version option is present, display version text and exit
   if(lget('today_test'))then                                  ! special option to list date format documentation
      call fmtdate_usage()                                     ! see all formatting options
   else
      options= sget('today_oo')                                ! get -oo STRING
      if(options == '')options='Y-M-D'                         ! if options are blank set a default
      write(*,'(a)')now(options)                               ! display current date using format from command line
   endif
end subroutine main
end program today

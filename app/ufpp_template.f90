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
'   prep_template(1f) - [PREP] write a template of a prep source file            ',&
'   (LICENSE:MIT)                                                                ',&
'SYNOPSIS                                                                        ',&
'   prep_template [ --version| --help]                                           ',&
'DESCRIPTION                                                                     ',&
'   A simple program that writes an example input file for prep(1).              ',&
'OPTIONS                                                                         ',&
'  --version  display version and quit                                           ',&
'  --help     display help text and quit                                         ',&
'EXAMPLE                                                                         ',&
'   prep_template                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   MIT License                                                                  ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    prep_template(1f) - [PREP] write a template of a prep source file
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    prep_template [ --version| --help]
!!##DESCRIPTION
!!    A simple program that writes an example input file for prep(1).
!!##OPTIONS
!!   --version  display version and quit
!!   --help     display help text and quit
!!##EXAMPLE
!!
!!    prep_template
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
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
'@(#)PROGRAM:        prep_template(1)>',&
'@(#)DESCRIPTION:    output a model of a prep(1) input file>',&
'@(#)VERSION:        1.0, 20180223>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2025-06-29 08:21:56 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program prep_template
use M_kracken, only : kracken, lget
implicit none

! ident_1="@(#) write a template of a prep source file"

integer :: i
character(len=:),allocatable :: example_text(:) ! this variable will be defined by $DOCUMENT VARIABLE directive
example_text=[ CHARACTER(LEN=128) :: &
'@!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'@IFDEF F90                                                                                                                ',&
'module example                                                                                                            ',&
'contains                                                                                                                  ',&
'end module example                                                                                                        ',&
'@ENDIF                                                                                                                    ',&
'@!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'@IFDEF TESTPRG90                                                                                                          ',&
'program krackenbasic                                                                                                      ',&
'use M_kracken, only : kracken, retrev, lget, rget, iget, sget, sgets                                                      ',&
'use M_strings, only : describe                                                                                            ',&
'use M_debug,   only : unit_test                                                                                           ',&
'use M_debug,   only : unit_test_start, unit_test_good                                                                     ',&
'implicit none                                                                                                             ',&
'                                                                                                                          ',&
'@@(#) module::name(3f) description                                                                                        ',&
'                                                                                                                          ',&
'integer,parameter                 :: dp=kind(0.0d0)                                                                       ',&
'   character(len=255)             :: filename                                                                             ',&
'   character(len=20)              :: device                                                                               ',&
'   character(len=256),allocatable :: words(:)                                                                             ',&
'   logical                        :: lval                                                                                 ',&
'   integer                        :: i_myloop                                                                             ',&
'   integer                        :: ier                                                                                  ',&
'   integer                        :: iflen                                                                                ',&
'   integer                        :: ival                                                                                 ',&
'   real                           :: rval                                                                                 ',&
'                                                                                                                          ',&
'!  define command arguments, default values and crack command line                                                        ',&
'   call kracken(''cmd'',''-i 10 -r 10e3 -l .false. -f input -d x11 -help .false. -version .false.'')                      ',&
'!  handle version and help requests                                                                                       ',&
'   call help_usage(lget("cmd_help"))                                                                                      ',&
'   call help_version(lget("cmd_version"))                                                                                 ',&
'                                                                                                                          ',&
'!  get values                                                                                                             ',&
'   call retrev(''cmd_f'',filename,iflen,ier) ! get -f FILENAME                                                            ',&
'   lval = lget(''cmd_l'')                    ! get -l present?                                                            ',&
'   rval = rget(''cmd_r'')                    ! get -r RVAL                                                                ',&
'   ival = iget(''cmd_i'')                    ! get -i IVAL                                                                ',&
'   device = sget(''cmd_d'')                  ! get -d STRING                                                              ',&
'   words = sgets(''cmd_d'')                  ! get -d STRING as an array of words                                         ',&
'!!                                                                                                                        ',&
'!  all done parsing; do something with the values                                                                         ',&
'   print *, "filename=",filename(:iflen)                                                                                  ',&
'   print *, " i=",ival, " r=",rval, " l=",lval                                                                            ',&
'   print *, " d="//device                                                                                                 ',&
'                                                                                                                          ',&
'   MYLOOP: do I_MYLOOP=1,10  ! DO loop                                                                                    ',&
'      cycle MYLOOP           ! start next pass of loop                                                                    ',&
'      exit  MYLOOP           ! go to next statement after corresponding ENDDO                                             ',&
'   enddo MYLOOP                                                                                                           ',&
'                                                                                                                          ',&
'   call execute_command_line(''goodbad xmple start'')                                                                     ',&
'   ! or                                                                                                                   ',&
'   call unit_test_start(''xmple'')                                                                                        ',&
'!!                                                                                                                        ',&
'   call unit_test(''xmple'',describe(char(126)).ne.''~ tilde'')                                                           ',&
'   ! or                                                                                                                   ',&
'   if (describe( char(126) ).ne. ''~ tilde'') then                                                                        ',&
'      write(*,*)''DESCRIBE OUTPUT=[''//describe(char(126))//'']''                                                         ',&
'      call execute_command_line(''goodbad xmple bad'')                                                                    ',&
'      stop 1                                                                                                              ',&
'   endif                                                                                                                  ',&
'!!                                                                                                                        ',&
'   ! if got here without being stopped assume passed test                                                                 ',&
'   call execute_command_line(''goodbad xmple good'')                                                                      ',&
'   ! or                                                                                                                   ',&
'   call unit_test_good(''xmple'')                                                                                         ',&
'                                                                                                                          ',&
'contains                                                                                                                  ',&
'                                                                                                                          ',&
'end program krackenbasic                                                                                                  ',&
'@DOCUMENT END                                                                                                             ',&
'@!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'@DOCUMENT HELP                                                                                                            ',&
'NAME                                                                                                                      ',&
'   name(3f) - [M_module] description                                                                                      ',&
'   (LICENSE:PD)                                                                                                           ',&
'SYNOPSIS                                                                                                                  ',&
'                                                                                                                          ',&
'DESCRIPTION                                                                                                               ',&
'EXAMPLE                                                                                                                   ',&
'AUTHOR                                                                                                                    ',&
'   John S. Urban                                                                                                          ',&
'LICENSE                                                                                                                   ',&
'   Public Domain                                                                                                          ',&
'@DOCUMENT END                                                                                                             ',&
'@!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'@DOCUMENT VERSION                                                                                                         ',&
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples                                                      ',&
'PROGRAM:        prep_template(1)                                                                                          ',&
'DESCRIPTION:    output a model of a prep(1) input file                                                                    ',&
'VERSION:        1.0, 20180223                                                                                             ',&
'AUTHOR:         John S. Urban                                                                                             ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html                                                            ',&
'@DOCUMENT END                                                                                                             ',&
'@!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',&
'@ENDIF                                                                                                                    ',&
'@ifdef PREP_TEST                                                                                                          ',&
'@!================================================================================================================        ',&
'@!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()        ',&
'@!================================================================================================================        ',&
'   This begins the section usually used for unit testing . Typically it is called by                                      ',&
'                                                                                                                          ',&
'      prep PREP_TEST -system -i FILENAME                                                                                  ',&
'                                                                                                                          ',&
'   if $SYSTEM commands are trusted                                                                                        ',&
'                                                                                                                          ',&
' Description of tests to                                                                                                  ',&
' be performed                                                                                                             ',&
'                                                                                                                          ',&
'@SYSTEM mkdir -p tmp/                                                                                                     ',&
'@SYSTEM goodbad xmple start -section 1                                                                                    ',&
'                                                                                                                          ',&
'        Make sure test executable does not exist.                                                                         ',&
'                                                                                                                          ',&
'        Test executable should start with underscore and be unique or you could remove a command                          ',&
'@SYSTEM rm -f `which __xmple 2>/dev/null||echo NOTTHERE`                                                                  ',&
'                                                                                                                          ',&
'        Build test program in standard location, assuming ccall(1) is available                                           ',&
'                                                                                                                          ',&
'@SYSTEM html2f90 < xmple.ff >tmp/__xmple.F90                                                                              ',&
'@SYSTEM ccall tmp/__xmple.F90                                                                                             ',&
'                                                                                                                          ',&
'        Execute test program and perform tests                                                                            ',&
'        The goodbad(1) creates an SQLite file with the status of the tests for                                            ',&
'        various programs and procedures.                                                                                  ',&
'                                                                                                                          ',&
'@SYSTEM __xmple && goodbad xmple good -section 1|| goodbad xmple bad -section 1                                           ',&
'                                                                                                                          ',&
'        Remove test source                                                                                                ',&
'                                                                                                                          ',&
'@SYSTEM rm tmp/__xmple.F90                                                                                                ',&
'                                                                                                                          ',&
'        Remove test executable                                                                                            ',&
'                                                                                                                          ',&
'@SYSTEM rm -f `which __xmple 2>/dev/null|| echo NOTTHERE`                                                                 ',&
'@!==================================================================================================================      ',&
'@!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()      ',&
'@!==================================================================================================================      ',&
'@ENDIF                                                                                                                    ',&
'']
   ! the rest of the program
   call kracken('cmd','-help .false. -version .f.')   ! define command arguments, default values and crack command line
   call help_usage(lget('cmd_help'))                  ! show help
   call help_version(lget('cmd_version'))             ! show version
   do i=1,size(example_text)                          ! replace leading @ with leading $ and print text
      if(example_text(i)(1:1).eq.'@') example_text(i)(1:1)='$'
      write(*,'(a)')trim(example_text(i))
   enddo
end program prep_template

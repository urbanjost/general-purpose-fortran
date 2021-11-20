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
'       false-(1f) - [FUNIX] do nothing, unsuccessfully                                                                          ',&
'       (LICENSE:PD)                                                                                                             ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'       false- value [ --verbose][ --help| --version]                                                                            ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'       Exit with a status code indicating failure.                                                                              ',&
'OPTIONS                                                                                                                         ',&
'       number     optional number of 1 to 32, which                                                                             ',&
'                  will be used to generate the exit                                                                             ',&
'                  status code if supported.                                                                                     ',&
'       --help     display this help and exit                                                                                    ',&
'       --version  output version information and exit                                                                           ',&
'       --verbose  display ASCII graphic of cockroach                                                                            ',&
'                                                                                                                                ',&
'EXAMPLE                                                                                                                         ',&
'      Bash example:                                                                                                             ',&
'                                                                                                                                ',&
'         false- || echo Should print this                                                                                       ',&
'                                                                                                                                ',&
'         if false-                                                                                                              ',&
'         then                                                                                                                   ',&
'            echo command got zero exit $?                                                                                       ',&
'         else                                                                                                                   ',&
'            echo command got non-zero exit $?                                                                                   ',&
'         fi                                                                                                                     ',&
'                                                                                                                                ',&
'      Expected output::                                                                                                         ',&
'                                                                                                                                ',&
'         ERROR STOP                                                                                                             ',&
'         Should print this                                                                                                      ',&
'         ERROR STOP                                                                                                             ',&
'         command got non-zero exit 1                                                                                            ',&
'                                                                                                                                ',&
'SEE ALSO                                                                                                                        ',&
'       _true(1f)                                                                                                                ',&
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
'@(#)PROGRAM:        false-(1f)>',&
'@(#)DESCRIPTION:    do nothing, unsuccessfully>',&
'@(#)VERSION:        1.0, 20170125>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain>',&
'@(#)COMPILED:       2021-11-20 15:29:15 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program false
use M_kracken,       only : kracken, lget, iget
use M_messages,      only : junroach
use M_verify,         only : fstop
use iso_fortran_env, only : ERROR_UNIT        ! access computing environment
implicit none
integer :: ios, istop

call kracken('false',' -help .F. -version .F. -verbose .F.')
call help_usage(lget('false_help'))      ! if -help option is present, display help text and exit
call help_version(lget('false_version')) ! if -version option is present, display version text and exit
if(lget('false_verbose'))then
   call junroach('s')
endif

istop=iget('false_oo')

select case(istop)
case(1:32)
   call fstop(istop)
case default
   !!error stop ''              ! get error returned to system and produce no message (hopefully -- compiler dependent)
   !!error stop                 ! get error returned to system and produce no message (hopefully -- compiler dependent)
   close(ERROR_UNIT,iostat=ios) ! try to stop default message
   stop 1
end select

end program false

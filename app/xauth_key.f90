!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
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
'   xauth_key(1f) - [M_random] generate magic cookies for xauth                                                                  ',&
'   (LICENSE:PD)                                                                                                                 ',&
'SYNOPSIS                                                                                                                        ',&
'   xauth_key [characters] [ -n number_of_characters]                                                                            ',&
'DESCRIPTION                                                                                                                     ',&
'   xauth_key generates a random hexadecimal number                                                                              ',&
'OPTIONS                                                                                                                         ',&
'   characters  Set of letters to compose string from.                                                                           ',&
'               Defaults to ''0123456789abcdef''.                                                                                ',&
'   -n N        Number of digits. Defaults to 128                                                                                ',&
'   -r N        Number of cookies to generate                                                                                    ',&
'   --version   Display version information and exit.                                                                            ',&
'   --help      Display help text and exit.                                                                                      ',&
'EXAMPLE                                                                                                                         ',&
'   generate a value for use with the X authority system:                                                                        ',&
'                                                                                                                                ',&
'    xauth add :0 . `xauth_key`                                                                                                  ',&
'SEE ALSO                                                                                                                        ',&
'   X(1), xauth(1)                                                                                                               ',&
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
!!    xauth_key(1f) - [M_random] generate magic cookies for xauth
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    xauth_key [characters] [ -n number_of_characters]
!!##DESCRIPTION
!!    xauth_key generates a random hexadecimal number
!!##OPTIONS
!!    characters  Set of letters to compose string from.
!!                Defaults to '0123456789abcdef'.
!!    -n N        Number of digits. Defaults to 128
!!    -r N        Number of cookies to generate
!!    --version   Display version information and exit.
!!    --help      Display help text and exit.
!!##EXAMPLE
!!
!!    generate a value for use with the X authority system:
!!
!!     xauth add :0 . `xauth_key`
!!##SEE ALSO
!!    X(1), xauth(1)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
program xauth_key
use M_kracken, only : kracken, lget, iget, sget
use M_random, only  : init_random_seed_by_dat
use M_random, only  : random_string, random_hex
implicit none
intrinsic random_number
integer :: length
character(len=:),allocatable :: out
character(len=:),allocatable :: chars
integer                      :: i
integer                      :: r
   call kracken('key','-n 128 -r 1 -help .false. -version .false.') ! define command arguments, default values; crack command line
   call help_usage(lget('key_help'))                           ! if -help option is present, display help text and exit
   call help_version(lget('key_version'))                      ! if -version option is present, display version text and exit
   length=iget('key_n')
   r=iget('key_r')
   chars=trim(sget('key_oo'))
   if(chars.eq.'')then
      chars='0123456789abcdef'
   endif

   call init_random_seed_by_dat()
   do i=1,r
      out=random_string(chars,length)
      write(*,'(a)')out
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
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
'@(#)PROGRAM:        xauth_key(1)>',&
'@(#)DESCRIPTION:    generate random strings such as magic cookies for xauth>',&
'@(#)VERSION:        1.0, 20171219>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-01-09 09:35:35 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program xauth_key
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

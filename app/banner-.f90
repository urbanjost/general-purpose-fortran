!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program block_letters
use M_kracken,  only : kracken, sget, lget, unnamed
use M_messages, only : signs
implicit none
character(len=1) :: letter
integer          :: i
   call kracken('banner','-c X -help .f. -version .f. ',style='args')    ! define and parse command line
   call help_usage(lget('banner_help'))                                  ! display help and stop if -help switch is present
   call help_version(lget('banner_version'))                             ! display version and stop if -version switch is present

   letter=sget('banner_c')
   do i=1,size(unnamed)
      call signs(unnamed(i),6,letter)
   enddo
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!----------------------------------------------------------------------------------------------------------------------------------!
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
'    banner-(1f) - [FUNIX] print large block letters                             ',&
'    (LICENSE:PD)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'  banner- [[-c LETTER] STRING(S)]| --help| --version                            ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'  Print strings as large block letters.                                         ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'  STRING(S)  strings to print as large block letters                            ',&
'  -c         letter to use to create block letters                              ',&
'  --help     display this help and exit                                         ',&
'  --version  output version information and exit                                ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  To generate a large banner enter                                              ',&
'                                                                                ',&
'   >banner- HELLO                                                               ',&
'   >                                                                            ',&
'   >  XXX XXX XXXXXXX XXXXX   XXXXX     XXX                                     ',&
'   >   X   X   X    X   X       X      X   X                                    ',&
'   >   X   X   X        X       X     X     X                                   ',&
'   >   X   X   X  X     X       X     X     X                                   ',&
'   >   XXXXX   XXXX     X       X     X     X                                   ',&
'   >   X   X   X  X     X       X     X     X                                   ',&
'   >   X   X   X        X       X     X     X                                   ',&
'   >   X   X   X    X   X   X   X   X  X   X                                    ',&
'   >  XXX XXX XXXXXXX XXXXXXX XXXXXXX   XXX                                     ',&
'                                                                                ',&
'   >banner -c L HELLO                                                           ',&
'   >  LLL LLL LLLLLLL LLLLL   LLLLL     LLL                                     ',&
'   >   L   L   L    L   L       L      L   L                                    ',&
'   >   L   L   L        L       L     L     L                                   ',&
'   >   L   L   L  L     L       L     L     L                                   ',&
'   >   LLLLL   LLLL     L       L     L     L                                   ',&
'   >   L   L   L  L     L       L     L     L                                   ',&
'   >   L   L   L        L       L     L     L                                   ',&
'   >   L   L   L    L   L   L   L   L  L   L                                    ',&
'   >  LLL LLL LLLLLLL LLLLLLL LLLLLLL   LLL                                     ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!----------------------------------------------------------------------------------------------------------------------------------!
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
'@(#)PROGRAM:        banner-(1f)>',&
'@(#)DESCRIPTION:    print text in big block letters>',&
'@(#)VERSION:        3.0, 20181028>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      Copyright (c) 1984, 1996 John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2024-12-14 14:58:24 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!----------------------------------------------------------------------------------------------------------------------------------!
end program block_letters
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

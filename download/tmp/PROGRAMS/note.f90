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
'    note - print large block letters                                            ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    note STRING(S) [-font FontName] |-test|-help|-version                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'    Print strings as large block letters using the blocks(3f) or                ',&
'    signs(3f) procedure.                                                        ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    STRING(S)  strings to print as large block letters                          ',&
'    --font alpha|banner  select font style                                      ',&
'    --test     the selected character set is printed, one letter at a time.     ',&
'    --help     display this help and exit                                       ',&
'    --version  output version information and exit                              ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'    To generate a large banner enter                                            ',&
'                                                                                ',&
'       note HELLO                                                               ',&
'                                                                                ',&
'       >  HH      HH  EEEEEEEEEE  LL          LL           OOOOOOOO             ',&
'       >  HH      HH  EEEEEEEEEE  LL          LL          OOOOOOOOOO            ',&
'       >  HH      HH  EE          LL          LL          OO     OOO            ',&
'       >  HH      HH  EE          LL          LL          OO    O OO            ',&
'       >  HHHHHHHHHH  EEEEE       LL          LL          OO   O  OO            ',&
'       >  HHHHHHHHHH  EEEEE       LL          LL          OO  O   OO            ',&
'       >  HH      HH  EE          LL          LL          OO O    OO            ',&
'       >  HH      HH  EE          LL          LL          OOO     OO            ',&
'       >  HH      HH  EEEEEEEEEE  LLLLLLLLLL  LLLLLLLLLL  OOOOOOOOOO            ',&
'       >  HH      HH  EEEEEEEEEE  LLLLLLLLLL  LLLLLLLLLL   OOOOOOOO             ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     note - print large block letters
!!
!!##SYNOPSIS
!!
!!     note STRING(S) [-font FontName] |-test|-help|-version
!!
!!##DESCRIPTION
!!     Print strings as large block letters using the blocks(3f) or
!!     signs(3f) procedure.
!!
!!##OPTIONS
!!     STRING(S)  strings to print as large block letters
!!     --font alpha|banner  select font style
!!     --test     the selected character set is printed, one letter at a time.
!!     --help     display this help and exit
!!     --version  output version information and exit
!!
!!##EXAMPLE
!!
!!     To generate a large banner enter
!!
!!        note HELLO
!!
!!        >  HH      HH  EEEEEEEEEE  LL          LL           OOOOOOOO
!!        >  HH      HH  EEEEEEEEEE  LL          LL          OOOOOOOOOO
!!        >  HH      HH  EE          LL          LL          OO     OOO
!!        >  HH      HH  EE          LL          LL          OO    O OO
!!        >  HHHHHHHHHH  EEEEE       LL          LL          OO   O  OO
!!        >  HHHHHHHHHH  EEEEE       LL          LL          OO  O   OO
!!        >  HH      HH  EE          LL          LL          OO O    OO
!!        >  HH      HH  EE          LL          LL          OOO     OO
!!        >  HH      HH  EEEEEEEEEE  LLLLLLLLLL  LLLLLLLLLL  OOOOOOOOOO
!!        >  HH      HH  EEEEEEEEEE  LLLLLLLLLL  LLLLLLLLLL   OOOOOOOO
!===================================================================================================================================
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        note(1f)>',&
'@(#)DESCRIPTION:    print alphabet in big block letters using blocks(3f) and signs(3f)>',&
'@(#)VERSION:        2.0, 20160624>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      Copyright (c) 1984, 1996 John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:03:52 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program block_letters
use M_kracken, only : kracken, sget, lget
use M_messages, only : blocks, signs
implicit none
character(len=1)    :: letter
integer             :: i
integer             :: ios
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('note','-help .f. -version .f. -test .f. -font alpha') ! define and parse command line
   call help_usage(lget('note_help'))                                  ! display help and stop if -help switch is present
   call help_version(lget('note_version'))                             ! display version and stop if -version switch is present
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget('note_test'))then                                           ! test switch is present, test blocks(3f)
      call callfont('TESTING')
      write(*,*)
      call callfont(sget('note_fonts'))
      write(*,*)
      do i=32,127                                                      ! display printable ASCII characters as block letters
         letter=char(i)
         write(*,*)'letter='//letter
         call callfont(letter)
         write(*,'(A)',advance='no')'Enter [RETURN] to continue'
         read(*,'(A)',iostat=ios)letter
         if(ios.ne.0)exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                                                ! display string from command line as block letters
      call callfont(sget('note_oo'))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   contains

   subroutine callfont(string)                                         ! based on font selected display string in block letters
   character(len=*) :: string
      select case (sget('note_font'))                                  ! determine selected font
      case('alpha')
         call blocks(string,6)                                         ! default font of block letters composed of associated letter
      case('banner')
         call signs(string,6)                                          ! alternate font simulates banner(1) command fonts
      case default                                                     ! unknown font name, still display with default
         write(*,*)'*note* unknown font'                               ! warn that font name is unknown
         call blocks(string,6)
      end select
   end subroutine callfont

end program block_letters

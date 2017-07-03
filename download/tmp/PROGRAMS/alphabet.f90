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
'   alphabet - print numeric values or a string as decimal, hexadecimal, octal and binary values',&
'SYNOPSIS                                                                        ',&
'   alphabet [values] [-h values][-z values][-o values][ -t text]                ',&
'DESCRIPTION                                                                     ',&
'   Write positive whole number 32-bit values (0 to 2147483647) in base          ',&
'   10(decimal), base 16(hexadecimal), base 2(binary) and base 8(octal)          ',&
'   Allowable range of values is from 0 to 2147483647 decimal.                   ',&
'                                                                                ',&
'   Alternatively, given a text string show the ASCII values for each            ',&
'   character in the string.                                                     ',&
'OPTIONS                                                                         ',&
'   value(s)  Values are treated as octal if they start with "o", binary         ',&
'             if they start with "b", and hexadecimal if they start with         ',&
'             "z" or "h". Otherwise, the values are assumed to be decimal.       ',&
'                                                                                ',&
'    -b values(s)     Values **without** a "b" prefix are assumed to be          ',&
'                     binary values.                                             ',&
'    -d values(s)     Values are assumed to be decimal values.                   ',&
'    -o values(s)     Values **without** an "o" prefix are assumed to be         ',&
'                     octal values.                                              ',&
'    -z|-h values(s)  Values **without** a "z" prefix are assumed to be          ',&
'                     hexadecimal values.                                        ',&
'    -t literal_text                                                             ',&
'    --help           display this help and exit                                 ',&
'    --version        output version information and exit                        ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
'  Sample commands:                                                              ',&
'                                                                                ',&
'   alphabet 2147483647 0  # decimal values                                      ',&
'   >Decimal    Hex         Octal          Binary                                ',&
'   >2147483647 Z"7FFFFFFF" O"17777777777" B"1111111111111111111111111111111"    ',&
'   >0          Z"0"        O"0"           B"0"                                  ',&
'                                                                                ',&
'   alphabet o144 z64 100 b1100100 # values with a prefix                        ',&
'   >Decimal    Hex         Octal          Binary                                ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'                                                                                ',&
'   alphabet -o 144 -h 64 -d 100 -b 1100100 # values with a keyword              ',&
'   >Decimal    Hex         Octal          Binary                                ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'   >100        Z"64"       O"144"         B"1100100"                            ',&
'                                                                                ',&
'   alphabet -t Hello # a string                                                 ',&
'   >Decimal    Hex         Octal          Binary     Description                ',&
'   >72         Z"48"       O"110"         B"1001000" majuscule H                ',&
'   >101        Z"65"       O"145"         B"1100101" miniscule e                ',&
'   >108        Z"6C"       O"154"         B"1101100" miniscule l                ',&
'   >108        Z"6C"       O"154"         B"1101100" miniscule l                ',&
'   >111        Z"6F"       O"157"         B"1101111" miniscule o                ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'   ascii(1),iprint(1)                                                           ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    alphabet - print numeric values or a string as decimal, hexadecimal, octal and binary values
!!##SYNOPSIS
!!
!!    alphabet [values] [-h values][-z values][-o values][ -t text]
!!##DESCRIPTION
!!    Write positive whole number 32-bit values (0 to 2147483647) in base
!!    10(decimal), base 16(hexadecimal), base 2(binary) and base 8(octal)
!!    Allowable range of values is from 0 to 2147483647 decimal.
!!
!!    Alternatively, given a text string show the ASCII values for each
!!    character in the string.
!!##OPTIONS
!!    value(s)  Values are treated as octal if they start with "o", binary
!!              if they start with "b", and hexadecimal if they start with
!!              "z" or "h". Otherwise, the values are assumed to be decimal.
!!
!!     -b values(s)     Values **without** a "b" prefix are assumed to be
!!                      binary values.
!!     -d values(s)     Values are assumed to be decimal values.
!!     -o values(s)     Values **without** an "o" prefix are assumed to be
!!                      octal values.
!!     -z|-h values(s)  Values **without** a "z" prefix are assumed to be
!!                      hexadecimal values.
!!     -t literal_text
!!     --help           display this help and exit
!!     --version        output version information and exit
!!##EXAMPLE
!!
!!
!!   Sample commands:
!!
!!    alphabet 2147483647 0  # decimal values
!!    >Decimal    Hex         Octal          Binary
!!    >2147483647 Z"7FFFFFFF" O"17777777777" B"1111111111111111111111111111111"
!!    >0          Z"0"        O"0"           B"0"
!!
!!    alphabet o144 z64 100 b1100100 # values with a prefix
!!    >Decimal    Hex         Octal          Binary
!!    >100        Z"64"       O"144"         B"1100100"
!!    >100        Z"64"       O"144"         B"1100100"
!!    >100        Z"64"       O"144"         B"1100100"
!!    >100        Z"64"       O"144"         B"1100100"
!!
!!    alphabet -o 144 -h 64 -d 100 -b 1100100 # values with a keyword
!!    >Decimal    Hex         Octal          Binary
!!    >100        Z"64"       O"144"         B"1100100"
!!    >100        Z"64"       O"144"         B"1100100"
!!    >100        Z"64"       O"144"         B"1100100"
!!    >100        Z"64"       O"144"         B"1100100"
!!
!!    alphabet -t Hello # a string
!!    >Decimal    Hex         Octal          Binary     Description
!!    >72         Z"48"       O"110"         B"1001000" majuscule H
!!    >101        Z"65"       O"145"         B"1100101" miniscule e
!!    >108        Z"6C"       O"154"         B"1101100" miniscule l
!!    >108        Z"6C"       O"154"         B"1101100" miniscule l
!!    >111        Z"6F"       O"157"         B"1101111" miniscule o
!!
!!##SEE ALSO
!!    ascii(1),iprint(1)
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
'@(#)PROGRAM:        alphabet(1f)>',&
'@(#)DESCRIPTION:    print a value as a decimal, hexadecimal, octal and binary value>',&
'@(#)VERSION:        1.0, 20160530>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 9:59:58 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program hexd
use m_kracken, only : kracken, sgets, lget, IPvalue, sget
use M_strings, only : describe
implicit none
character(len=IPvalue),allocatable :: words(:)
character(len=:),allocatable       :: string
integer                :: int, i, isum=0
logical                :: describe_text
integer                :: stringlen
integer                :: intpos
!===================================================================================================================================
   call kracken('alphabet',' -t -h -o -b -d -help .false. -version .false.') ! define can crack command line
   call help_usage(lget('alphabet_help'))                                    ! check for help text
   call help_version(lget('alphabet_version'))                               ! check for version text
   call process_option('alphabet_h','Z20')                                   ! process command line switches
   call process_option('alphabet_d','I20')
   call process_option('alphabet_o','O20')
   call process_option('alphabet_b','b20')
!===================================================================================================================================
   words=sgets('alphabet_oo')                                                ! the values without a keyword
   if(size(words).gt.0)then
      write(*,'(a)')' Decimal    Hex         Octal          Binary'
      isum=isum+size(words)
   endif
   do i=1,size(words)
      select case(words(i)(1:1))
      case('z','Z','h','H')
         read(words(i)(2:),'(Z20)',err=999)int
      case('b','B')
         read(words(i)(2:),'(B20)',err=999)int
      case('O','o')
         read(words(i)(2:),'(O20)',err=999)int
      case default
         read(words(i),'(I20)',err=999)int
      end select
      intpos=iabs(int)
      write(*,202)intpos,intpos,intpos,intpos,''
      cycle
      999 continue
      write(*,*)'error reading value from '//trim(words(i))
   enddo
202 format(1x,i0,T13,"Z",'"',z0,'"',T25,"O",'"',o0,'"',T40,"B",'"',b0,'"',T76,a)
!===================================================================================================================================
   string=trim(sget('alphabet_t'))                                 ! print ascii string character by character
   stringlen=len(string)                                           ! get number of characters to display in string
   if(stringlen.ne.0) then                                         ! if the string is not empty print it
      write(*,'(a)')' Decimal    Hex         Octal          Binary     Description'
      isum=isum+stringlen                                          ! set flag to indicate not to print table of all characters
      describe_text=.true.                                         ! set flag to print character description with values
      do i=1,stringlen                                             ! step through string one character at a time
         int=ichar(string(i:i))                                    ! get ASCII Decimal Number for each character
         intpos=iabs(int)
         write(*,303)intpos,intpos,intpos,intpos,describe(char(intpos))
      enddo
   endif
303 format(1x,i0,T13,"Z",'"',z0,'"',T25,"O",'"',o0,'"',T40,"B",'"',b0,'"',1x,a)
!===================================================================================================================================
   if(isum.eq.0)then                                               ! if no values printed using other options print table
      write(*,'(a)')' Decimal    Hex   Octal     Binary Description'
      do int=0,127
         write(*,101)int,int,int,int,describe(char(int))
      enddo
   endif
   101 format(2x,i6,1x,z6,1x,o7,1x,b10,1x,a)
!===================================================================================================================================
   stop
!===================================================================================================================================
contains
!===================================================================================================================================
subroutine process_option(option,fmt)                            ! given option name and format for reading number read value
character(len=*) :: option, fmt
integer          :: ios
   words=sgets(option)
   if(isum.eq.0.and.size(words).ne.0)then                             ! if first time there is something to print
      write(*,'(a)')' Decimal    Hex         Octal          Binary'
   endif
   isum=isum+size(words)
   do i=1,size(words)
      read(words(i),'('//fmt//')',iostat=ios)int
      if(ios.ne.0)then
         write(*,*)'error reading value from '//trim(words(i))//' using format '//trim(fmt)
      else
         intpos=iabs(int)
         write(*,404)intpos,intpos,intpos,intpos,'' ! print ADE value of INT as decimal, hexadecimal, octal, and binary
      endif
   enddo
404 format(1x,i0,T13,"Z",'"',z0,'"',T25,"O",'"',o0,'"',T40,"B",'"',b0,'"',T76,a)
end subroutine process_option
!===================================================================================================================================
end program hexd
!===================================================================================================================================

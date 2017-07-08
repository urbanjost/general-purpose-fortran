!-------------------------------------------------------------------------------
! Purpose: An example of a simple utility that uses stream I/O from Fortran 2003
!-------------------------------------------------------------------------------
! REQUIRES (for cracking command line arguments):
!#URL http://home.comcast.net/~urbanjost/LIBRARY/libjust4/arguments/src/M_kracken.f90
!-------------------------------------------------------------------------------
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
'       dtu(1) - [FILE FILTER]convert files between Unix and DOS line terminator conventions',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       dtu [[-make dos|unix] [-z] [-n] -i input -o output ]|--help|--version    ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       Convert DOS end-of-line (CR-LF or <carriage-return><line-feed>)          ',&
'       to a Unix end-of-line (LF or <line-feed>, often called "newline");       ',&
'       or vice-versa.                                                           ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       -make unix      (default) convert DOS file to Unix ( CR-LF to newline )  ',&
'       -make dos       convert Unix file to DOS ( newline to CR-LF )            ',&
'       -z              guarantee last character of DOS file is ^Z,              ',&
'                       guarantee last character of Unix file is not ^Z          ',&
'                       otherwise, ^Z in input is copied or not as-is            ',&
'       -n              noisy mode reports character and line counts on stderr   ',&
'       -i input_file   required                                                 ',&
'       -o output_file  required                                                 ',&
'       --help          display this help and exit                               ',&
'       --version       output version information and exit                      ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        dtu(1) - [FILE FILTER]convert files between Unix and DOS line terminator conventions
!!
!!##SYNOPSIS
!!
!!        dtu [[-make dos|unix] [-z] [-n] -i input -o output ]|--help|--version
!!
!!##DESCRIPTION
!!        Convert DOS end-of-line (CR-LF or <carriage-return><line-feed>)
!!        to a Unix end-of-line (LF or <line-feed>, often called "newline");
!!        or vice-versa.
!!
!!##OPTIONS
!!        -make unix      (default) convert DOS file to Unix ( CR-LF to newline )
!!        -make dos       convert Unix file to DOS ( newline to CR-LF )
!!        -z              guarantee last character of DOS file is ^Z,
!!                        guarantee last character of Unix file is not ^Z
!!                        otherwise, ^Z in input is copied or not as-is
!!        -n              noisy mode reports character and line counts on stderr
!!        -i input_file   required
!!        -o output_file  required
!!        --help          display this help and exit
!!        --version       output version information and exit
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
'@(#)PROGRAM:        dtu(1f)>',&
'@(#)DESCRIPTION:    convert files between Unix and DOS line terminator conventions>',&
'@(#)VERSION:        1.0 Mon. Jun 22, 2009>',&
'@(#)VERSION:        2.0 Mon. Nov 24, 2014>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        This program is free software; you can redistribute it and/or>',&
'@(#)                modify it.>',&
'@(#)>',&
'@(#)                This program is distributed in the hope that it will be useful,>',&
'@(#)                but WITHOUT ANY WARRANTY; without even the implied warranty of>',&
'@(#)                MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:02:05 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
module GLOBAL
implicit none

   ! constants
   character(len=1),parameter ::  CZ=char(26)     ! DOS eof (ctrl-Z)
   character(len=1),parameter ::  NL=char(10)     ! Unix end of line
   character(len=1),parameter ::  CR=char(13)     ! DOS carriage-return
   character(len=1),parameter ::  LF=char(10)     ! DOS line-feed

   ! global variables
   ! input & output files
   integer :: IUNIT=15                            ! input file unit
   integer :: OUNIT=16                            ! output file unit

   ! flags indicating command line options
   logical  :: process_ctrl_z = .false.           ! process ^Z or not
   logical  :: noisy = .false.                    ! print report or not

   ! character and line counts
   integer  :: chars_read = 0                     ! count chars read
   integer  :: chars_written = 0                  ! count chars written
   integer  :: lines_written = 0                  ! count lines
   contains
   !-------------------------------------------------------------------------------
   integer function readchar(char1) ! read a single character from the stream
      character(len=1) :: char1
      read(iunit,iostat=readchar)char1
      if(readchar == 0) chars_read=chars_read+1                     ! count chars
   end function readchar
   !-------------------------------------------------------------------------------
   integer function writechar(char1) result (ios) ! write a single character from the stream
      character(len=1) :: char1
      write(ounit,iostat=ios)char1
      if(ios == 0) chars_written=chars_written+1            ! count chars
   end function writechar
   !-------------------------------------------------------------------------------
end module global
!-------------------------------------------------------------------------------
! procedures
!-------------------------------------------------------------------------------
subroutine dos_to_unix()                          ! copy CR-LF to newline
use global
use iso_fortran_env, only : error_unit            ! access computing environment
implicit none
   character(len=1) :: c                          ! character to be copied
   character(len=1) :: prev_c=" "                 ! look ahead character
   integer          :: ios
   integer          :: idum
   if( noisy ) write(ERROR_UNIT,'(a)')" make: dos to unix"
   ios=readchar(prev_c)                           ! start prev_c, c pipeline
   if( ios /= 0 )then   ! check eof
      write(ERROR_UNIT,'(a)')" *dtu* Warning: Empty input file"
      stop 1                                      ! quit if no work
   endif
   do                                             ! copy character by character
      ios=readchar(c)                             ! read to eof
      if(ios /= 0 ) exit                          ! if end of file or error quit reading
      ! check for a CR-LF sequence
      if( (prev_c == CR) .and. (c == LF) )then    ! found CR-LF
         idum=writechar(NL)
         lines_written=lines_written+1            ! count lines
         ios=readchar(c)                          ! reload pipeline
         if( ios /= 0 )then                       ! check eof
            prev_c = c                            ! set flag
            exit                                  ! quit at eof
         endif
      else                                        ! any other
         idum=writechar(prev_c)                   ! write char
      endif
      prev_c = c                                  ! cycle pipeline
   enddo
                                                  ! ctrl-Z as-is
   if( process_ctrl_z)then                        ! write last character
      if( prev_c /= CZ ) then
         idum=writechar(prev_c)                   ! write char
      endif
   else                                           ! guarantee no ^Z
      if((prev_c /= CZ)) then                     ! char is not ^Z
         idum=writechar(prev_c)                   ! write char
      endif
   endif
end subroutine dos_to_unix
!-------------------------------------------------------------------------------
subroutine unix_to_dos()                          ! copy newline to CR-LF
use GLOBAL
use iso_fortran_env, only : ERROR_UNIT            ! access computing environment
implicit none
   character(len=1) :: c                          ! character to be copied
   character(len=1) :: prev_c=" "
   integer          :: idum
   integer          :: ios
   if(noisy)write(ERROR_UNIT,'(a)')" make: unix to dos"
   do                                             ! copy character by character
      ios=readchar(c)                             ! read to eof
      if( ios /= 0 ) exit                         ! quit reading on error or eof (^better if distinguish)
      if( c == NL ) then                          ! if newline
         idum=writechar(CR)                       ! write CR
         idum=writechar(LF)                       ! write LF
         lines_written=lines_written+1            ! count line
      else                                        ! any other char
         idum=writechar(c)                        ! write char
      endif
      prev_c = c                                  ! check to guarantee ^Z
   enddo
                                                  ! complain if input empty
   if( chars_read == 0 ) then                     ! nothing was read
      write(ERROR_UNIT,'(a)')" *dtu* Warning: Empty input file"
      stop 2                                      ! quit if no work
   endif
                                                  ! check last character
   if( process_ctrl_z)then                        ! guarantee ^Z
      if( prev_c /= CZ ) then                     ! last char is not ^Z
         idum=writechar(CZ)                       ! write CZ
      endif
   endif
end subroutine unix_to_dos
!-------------------------------------------------------------------------------
! read command line, process file and optionally print statistics
program dtu_exe
use GLOBAL
use M_kracken, only: IPvalue,kracken,lget
implicit none

   call kracken('dtu','          &
   & -i "-"                      &
   & -o "-"                      &
   & -make u                     &
   & -z .false.                  &
   & -n .false. -noisy   .false. &
   & -v .false. -version .false. &
   & -h .false. -help .false.    &
   & ')                                           ! crack command-line in general
   call help_usage(lget('dtu_help'))              ! check if help requested
   call help_version(lget('dtu_version'))         ! check if version requested

   process_ctrl_z= lget('dtu_z')                  ! set flag to force ctrl-Z processing

   if(lget('dtu_n').or.lget('dtu_noisy'))then
      noisy= .true.                               ! report character and line counts
   else
      noisy= .false.
   endif

   call writefile()                               ! use -i and -o

end program dtu_exe
!-------------------------------------------------------------------------------
subroutine writefile() ! given an input and output file, read the input file and write the output file
use iso_fortran_env, only : ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT ! access computing environment
use global
use M_kracken,only: IPvalue, sget
implicit none
   character(len=IPvalue)  :: input                ! input file
   character(len=IPvalue)  :: output               ! output file

   input=sget('dtu_i')                             ! get -i FILENAME
   select case(input)
   case('-',' ')
      IUNIT=INPUT_UNIT
      ! how do you reassign stdin to be stream?
      ! open(unit=IUNIT,access="stream")
      write(ERROR_UNIT,*)'-i INPUTFILE required'
      stop 2
   case default
      IUNIT=15
      open(unit=IUNIT,file=trim(input),status="old",access="stream")
   end select

   output=sget('dtu_o')                            ! get -o FILENAME
   select case(output)
   case('-',' ')
      OUNIT=OUTPUT_UNIT
      ! how do you reassign stdin to be stream?
      ! open(unit=OUNIT,access="stream",form="unformatted")
      write(ERROR_UNIT,*)'-l OUTPUTFILE required'
      stop 3
   case default
      OUNIT=16
      open(unit=OUNIT,file=trim(output),status="replace",access="stream")
   end select

   if (sget('dtu_make') == 'dos' ) then             ! process DOS file to Unix or Unix to DOS
      call unix_to_dos()                            ! Unix to DOS
   else
      call dos_to_unix()                            ! DOS to Unix
   endif

   if(noisy)then
      write(ERROR_UNIT,*)"input: ",IUNIT,":",trim(input)
      write(ERROR_UNIT,*)"output:",OUNIT,":",trim(output)
      write(ERROR_UNIT,*)"read:  ",chars_read," written: ",chars_written," lines: ",lines_written ! report counts
   endif
end subroutine writefile
!-------------------------------------------------------------------------------

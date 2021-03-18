subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   reverse(1f) - [FUNIX] print file in reverse                                  ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   reverse INPUT_FILE [OUTPUT_FILE] [ --help][ --version]                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Read entire file into memory as a stream and write it in reverse             ',&
'   byte order                                                                   ',&
'OPTIONS                                                                         ',&
'   INPUT_FILE   input file                                                      ',&
'   OUTPUT_FILE  output file                                                     ',&
'   --help       display help text and exit                                      ',&
'   --version    display version information and exit                            ',&
'SEE ALSO                                                                        ',&
'   reverse(1), rev(1)                                                           ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    reverse(1f) - [FUNIX] print file in reverse
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    reverse INPUT_FILE [OUTPUT_FILE] [ --help][ --version]
!!
!!##DESCRIPTION
!!    Read entire file into memory as a stream and write it in reverse
!!    byte order
!!##OPTIONS
!!    INPUT_FILE   input file
!!    OUTPUT_FILE  output file
!!    --help       display help text and exit
!!    --version    display version information and exit
!!##SEE ALSO
!!    reverse(1), rev(1)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
! @(#)help_version(3f): prints version information
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        reverse(1f)>',&
'@(#)DESCRIPTION:    read entire file into memory as a stream and write it in reverse byte order>',&
'@(#)VERSION:        1.0, 2009-06-26>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      (C) 2009 John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Mon, Mar 15th, 2021 12:51:00 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program rever
!#(@)read entire file into memory as a stream and write it in reverse byte order
! example of using Fortran stream I/O
! ??? - how to make stdin and stdout and stderr stream files  ???
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
use iso_fortran_env, only : output_unit                                          ! access computing environment
use M_io, only      : slurp, notopen
use M_verify, only   : stderr
use M_kracken, only : kracken, sgets, lget, IPvalue
use M_strings, only : split
implicit none
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
   character(len=1),allocatable :: text(:)                                       ! array to hold file in memory
   integer :: ios                                                                ! I/O error flag
   integer :: iputunit                                                           ! unit number for output file
   character(len=IPvalue),allocatable :: files(:)                                ! array to hold files from command line
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
   call kracken('reverse',' -help .false. -version .false.')                     ! crack command line
   call help_usage(lget('reverse_help'))                                         ! check if -help present
   call help_version(lget('reverse_version'))                                    ! check if -version present
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
   files=sgets('reserve_oo')                                                     ! get filename(s) from command line
   select case(size(files))
   case(:0,3:)                                                                   ! unsupported input syntax.
      call stderr('*reverse* usage: reverse inputfile [outputfile]')             ! show acceptable usage and quit
      stop
   case(1)                                                                       ! input but not output filename specified
      iputunit=OUTPUT_UNIT                                                       ! use stdout as the output file
   case(2)                                                                       ! input and output filenames specified
      iputunit=notopen(10,99)                                                    ! get unit number for output file
      if(iputunit.lt.0)then
         call stderr('*reverse* could not find unused file unit number for output')
         stop
      endif
      open (unit=iputunit,      &                                                ! open named file as output in stream mode
      & file=trim(files(2)),    &
      & access='stream')
   end select
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
   call slurp(files(1),text)                                                     ! allocate character array and copy file into it
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
   if(.not.allocated(text))then
      call stderr('*reverse* failed to load file '//trim(files(1)))
      close(iputunit, iostat=ios)
      stop
!- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
   elseif(size(files).eq.1)then
      write(*,'(*(a:))',advance='no')text(size(text):1:-1)                  ! KLUDGE: write file reversed to a non-preassigned file
   else
      write(iputunit)text(size(text):1:-1)                                  ! write file reversed to stdout
      close(iputunit, iostat=ios)
   endif
   deallocate(text)                                                         ! release memory
end program rever

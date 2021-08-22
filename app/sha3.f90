!>
!!##NAME
!!    sha3(1) - [FUNIX:M_strings] generate SHA-256 digest values for specified files
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        sha3 [-224|-256|-384|-512|-a] FILE...
!!
!!##DESCRIPTION
!!    Example of using M_sha3(3fm) module. Calculates SHA digest values for specified files.
!!
!!    NOT CURRENTLY WORKING
!!
!!##OPTIONS
!!    -224     calculate SHA-224 digest values for specified files
!!    -256     calculate SHA-224 digest values for specified files(default)
!!    -384     calculate SHA-224 digest values for specified files
!!    -512     calculate SHA-224 digest values for specified files
!!    -a       run internal tests of routines in M_sha3(3fm) module
!!    FILE(S)  names of files to generate a hash for.
!!
!!##EXAMPLES
!!
!!   Sample usage
!!      sha3 *
program demo_M_sha3
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_sha3,                       only : sha3_auto_test, sha3_file
use M_system,                     only : system_isreg
implicit none

! ident_1="@(#)sha3(1f): generate SHA-256 digest values for specified files"

integer                      :: i
integer                      :: start
character(len=4096)          :: fname, arg

   call get_command_argument( 1, arg )
   if ( trim(arg) .eq. '-a' ) then
      call sha3_auto_test()
   elseif ( arg(1:1) .eq. '-' ) then
      start=2
   else
      start=1
      arg='-256'
   endif
   do i=start,command_argument_count() ! step through filenames on command line
      call get_command_argument( i, fname )
      if(.not.system_isreg(fname))cycle
      select case(arg)
       case( '-224' ) ; call sha3_file( 224, trim(fname) )
       case( '-256' ) ; call sha3_file( 256, trim(fname) )
       case( '-384' ) ; call sha3_file( 384, trim(fname) )
       case( '-512' ) ; call sha3_file( 512, trim(fname) )
       case default
         write(ERROR_UNIT,*)'"usage: "sha3 -a" or "sha3 [ -224| -256| -384| -512] fname"'
      end select
   enddo

end program demo_M_sha3

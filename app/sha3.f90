program demo_M_sha3

! ident_1="@(#)sha3(1f): generate SHA-{224,256,384,512} digest values for specified files"

use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_sha3,                       only : sha3_auto_test, sha3_file
use M_system,                     only : system_isreg
use M_CLI2,                       only : set_args, iget, lget, files=>unnamed
implicit none
integer                      :: i, ibitsz
character(len=:),allocatable :: fname
character(len=:),allocatable :: help_text(:), version_text(:)

   call setup()
   call set_args(' --bits:b 256 --auto_test:a F',help_text,version_text)
   ibitsz=iget('bits')
   if (lget('auto_test'))then
      call sha3_auto_test()
   endif
   do i=1,size(files) ! step through filenames on command line
      fname=trim(files(i))
      if(.not.system_isreg(fname))cycle
      select case(ibitsz)
       case( 224 , 256, 384 , 512 ) ; call sha3_file( ibitsz, fname)
       case default                 ; write(ERROR_UNIT,*)'"usage: "sha3 -a" or "sha3 [ -bits [224|256|384|512] fname(s)"'
      end select
   enddo

contains

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   sha3(1) - [FUNIX:M_strings] generate SHA-{224,256,384,512} digest',&
'   values for specified files',&
'   (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'       sha3 [--bits [224|256|384|512] [--auto_test] FILE...',&
'',&
'DESCRIPTION',&
'   Example of using M_sha3(3fm) module. Calculates SHA digest values',&
'   for specified files.',&
'',&
'   NOT CURRENTLY WORKING',&
'',&
'OPTIONS',&
' --bits,b  NNN where NNN is the digest value size in bits.',&
'',&
'             +224   calculate SHA-224 digest values for specified files',&
'             +256   calculate SHA-256 digest values for specified files',&
'                    (default)',&
'             +384   calculate SHA-384 digest values for specified files',&
'             +512   calculate SHA-512 digest values for specified files',&
'',&
' --auto_test,a      run internal tests of routines in M_sha3(3fm) module',&
'',&
'   FILE(S)          names of files to generate a hash for.',&
'',&
'EXAMPLES',&
'    Sample usage',&
'',&
'      sha3 *',&
'      sha3 --bits 512 *',&
'',&
'']
!>
!!##NAME
!!    sha3(1) - [FUNIX:M_strings] generate SHA-{224,256,384,512} digest
!!    values for specified files
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        sha3 [--bits [224|256|384|512] [--auto_test] FILE...
!!
!!##DESCRIPTION
!!    Example of using M_sha3(3fm) module. Calculates SHA digest values
!!    for specified files.
!!
!!    NOT CURRENTLY WORKING
!!
!!##OPTIONS
!!  --bits,b  NNN where NNN is the digest value size in bits.
!!
!!              +224   calculate SHA-224 digest values for specified files
!!              +256   calculate SHA-256 digest values for specified files
!!                     (default)
!!              +384   calculate SHA-384 digest values for specified files
!!              +512   calculate SHA-512 digest values for specified files
!!
!!  --auto_test,a      run internal tests of routines in M_sha3(3fm) module
!!
!!    FILE(S)          names of files to generate a hash for.
!!
!!##EXAMPLES
!!
!!     Sample usage
!!
!!       sha3 *
!!       sha3 --bits 512 *
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        sha3(1)>',&
'@(#)DESCRIPTION:    generate SHA digest values for specified files>',&
'@(#)VERSION:        1.0-20220715>',&
'@(#)AUTHOR:         John S. Urban>',&
'']
end subroutine setup

end program demo_M_sha3

